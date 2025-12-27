{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Crypto.MAC.Poly1305
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A pure Poly1305 MAC implementation, as specified by
-- [RFC 8439](https://datatracker.ietf.org/doc/html/rfc8439).

module Crypto.MAC.Poly1305 (
    -- * Poly1305 message authentication code
    mac

    -- testing
  , _poly1305_loop
  , _roll16
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Data.Word.Limb (Limb(..))
import qualified Data.Word.Limb as L
import Data.Word.Wider (Wider(..))
import qualified Data.Word.Wider as W
import qualified Foreign.Storable as Storable (pokeByteOff)
import qualified GHC.Exts as Exts
import qualified GHC.Word (Word8(..))

-- utilities ------------------------------------------------------------------

-- convert a Word8 to a Limb
limb :: Word8 -> Limb
limb (GHC.Word.W8# (Exts.word8ToWord# -> w)) = Limb w
{-# INLINABLE limb #-}

-- convert a Limb to a Word8
word8 :: Limb -> Word8
word8 (Limb w) = GHC.Word.W8# (Exts.wordToWord8# w)
{-# INLINABLE word8 #-}

-- convert a Limb to a Word8 after right-shifting
word8s :: Limb -> Exts.Int# -> Word8
word8s l s =
  let !(Limb w) = L.shr# l s
  in  GHC.Word.W8# (Exts.wordToWord8# w)
{-# INLINABLE word8s #-}

-- 128-bit little-endian bytestring decoding
_roll16 :: BS.ByteString -> Wider
_roll16 bs@(BI.PS _ _ l) =
  let byte :: Int -> Limb
      byte i
        | i < l     = limb (BU.unsafeIndex bs i)
        | otherwise = Limb 0##
      {-# INLINE byte #-}
      !w0 =     (byte 07 `L.shl#` 56#)
        `L.or#` (byte 06 `L.shl#` 48#)
        `L.or#` (byte 05 `L.shl#` 40#)
        `L.or#` (byte 04 `L.shl#` 32#)
        `L.or#` (byte 03 `L.shl#` 24#)
        `L.or#` (byte 02 `L.shl#` 16#)
        `L.or#` (byte 01 `L.shl#` 08#)
        `L.or#` byte 00
      !w1 =     (byte 15 `L.shl#` 56#)
        `L.or#` (byte 14 `L.shl#` 48#)
        `L.or#` (byte 13 `L.shl#` 40#)
        `L.or#` (byte 12 `L.shl#` 32#)
        `L.or#` (byte 11 `L.shl#` 24#)
        `L.or#` (byte 10 `L.shl#` 16#)
        `L.or#` (byte 09 `L.shl#` 08#)
        `L.or#` byte 08
  in  Wider (# w0, w1, Limb 0##, Limb 0## #)
{-# INLINE _roll16 #-}

-- 128-bit little-endian bytestring encoding
unroll16 :: Wider -> BS.ByteString
unroll16 (Wider (# w0, w1, _, _ #)) =
  BI.unsafeCreate 16 $ \ptr -> do
    -- w0
    Storable.pokeByteOff ptr 00 (word8 w0)
    Storable.pokeByteOff ptr 01 (word8s w0 08#)
    Storable.pokeByteOff ptr 02 (word8s w0 16#)
    Storable.pokeByteOff ptr 03 (word8s w0 24#)
    Storable.pokeByteOff ptr 04 (word8s w0 32#)
    Storable.pokeByteOff ptr 05 (word8s w0 40#)
    Storable.pokeByteOff ptr 06 (word8s w0 48#)
    Storable.pokeByteOff ptr 07 (word8s w0 56#)
    -- w1
    Storable.pokeByteOff ptr 08 (word8 w1)
    Storable.pokeByteOff ptr 09 (word8s w1 08#)
    Storable.pokeByteOff ptr 10 (word8s w1 16#)
    Storable.pokeByteOff ptr 11 (word8s w1 24#)
    Storable.pokeByteOff ptr 12 (word8s w1 32#)
    Storable.pokeByteOff ptr 13 (word8s w1 40#)
    Storable.pokeByteOff ptr 14 (word8s w1 48#)
    Storable.pokeByteOff ptr 15 (word8s w1 56#)
{-# INLINABLE unroll16 #-}

-- set high bit for chunk of length l (max 16)
set_hi :: Int -> Wider
set_hi l
  | l < 8     = W.shl_limb 1 (8 * l)
  | l < 16    = Wider (# Limb 0##, L.shl# (Limb 1##) s, Limb 0##, Limb 0## #)
  | otherwise = Wider (# Limb 0##, Limb 0##, Limb 1##, Limb 0## #)
  where
    !(Exts.I# s) = 8 * (l - 8)
{-# INLINE set_hi #-}

-- bespoke constant-time 130-bit right shift
shr130 :: Wider -> Wider
shr130 (Wider (# _, _, l2, l3 #)) =
  let !r0 = L.or# (L.shr# l2 2#) (L.shl# l3 62#)
      !r1 = L.shr# l3 2#
  in  Wider (# r0, r1, Limb 0##, Limb 0## #)
{-# INLINE shr130 #-}

-------------------------------------------------------------------------------

clamp :: Wider -> Wider
clamp r = r `W.and` 0x0ffffffc0ffffffc0ffffffc0fffffff
{-# INLINE clamp #-}

-- | Produce a Poly1305 MAC for the provided message, given the provided
--   key.
--
--   Per RFC8439: the key, which is essentially a /one-time/ key, should
--   be unique, and MUST be unpredictable for each invocation.
--
--   The key must be exactly 256 bits in length.
--
--   >>> mac "i'll never use this key again!!!" "a message needing authentication"
--   Just "O'\231Z\224\149\148\246\203[}\210\203\b\200\207"
mac
  :: BS.ByteString -- ^ 256-bit one-time key
  -> BS.ByteString -- ^ arbitrary-length message
  -> Maybe BS.ByteString -- ^ 128-bit message authentication code
mac key@(BI.PS _ _ kl) msg
  | kl /= 32  = Nothing
  | otherwise =
      let (clamp . _roll16 -> r, _roll16 -> s) = BS.splitAt 16 key
      in  pure (_poly1305_loop r s msg)

-- p = 2^130 - 5
--
-- mask for the low 130 bits
mask130 :: Wider
mask130 = 0x3ffffffffffffffffffffffffffffffff
{-# INLINE mask130 #-}

-- partial reduction to [0, 2 ^ 131)
reduce_partial :: Wider -> Wider
reduce_partial x =
  let !lo = x `W.and` mask130
      !hi = shr130 x
  in  lo + 5 * hi
{-# INLINE reduce_partial #-}

-- [0, 2 ^ 131) -> [0, p)
reduce_full :: Wider -> Wider
reduce_full h =
  let !lo = h `W.and` mask130
      !hi  = shr130 h
      !h'  = lo + 5 * hi
      !h_5 = h' + 5
      !reduced = h_5 `W.and` mask130
      !carry   = shr130 h_5
      !gte     = W.lt 0 carry
  in  W.select h' reduced gte
{-# INLINE reduce_full #-}

_poly1305_loop :: Wider -> Wider -> BS.ByteString -> BS.ByteString
_poly1305_loop !r !s !msg =
    let loop !acc !bs = case BS.splitAt 16 bs of
          (chunk@(BI.PS _ _ l), etc)
            | l == 0 ->
                let !final = reduce_full (reduce_partial acc)
                in  unroll16 (final + s)
            | otherwise ->
                let !n = _roll16 chunk `W.or` set_hi l
                    !prod = r * (acc + n)
                    !nacc = reduce_partial (reduce_partial prod)
                in  loop nacc etc
    in  loop 0 msg
{-# INLINE _poly1305_loop #-}


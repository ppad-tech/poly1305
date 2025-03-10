{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

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
  ) where

import Data.Bits ((.&.), (.|.), (.<<.), (.>>.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- arbitrary-size little-endian bytestring decoding
roll :: BS.ByteString -> Integer
roll = BS.foldr alg 0 where
  alg (fi -> !b) !a = (a .<<. 8) .|. b
{-# INLINE roll #-}

-- little-endian bytestring encoding
unroll :: Integer -> BS.ByteString
unroll i = case i of
    0 -> BS.singleton 0
    _ -> BS.unfoldr coalg i
  where
    coalg = \case
      0 -> Nothing
      m -> Just $! (fi m, m .>>. 8)
{-# INLINE unroll #-}

-- little-endian bytestring encoding for 128-bit ints, right-padding
-- with zeros
unroll16 :: Integer -> BS.ByteString
unroll16 (unroll -> u@(BI.PS _ _ l))
  | l < 16 = u <> BS.replicate (16 - l) 0
  | otherwise = u
{-# INLINE unroll16 #-}

clamp :: Integer -> Integer
clamp r = r .&. 0x0ffffffc0ffffffc0ffffffc0fffffff
{-# INLINE clamp #-}

-- | Produce a Poly1305 MAC for the provided message, given the provided
--   key.
--
--   Per RFC8439: the key, which is essentially a /one-time/ key, should
--   be unique, and MUST be unpredictable for each invocation.
--
--   The key must be exactly 256 bits in length. Providing an invalid
--   key will cause the function to throw an ErrorCall exception.
--
--   >>> mac "i'll never use this key again!!!" "a message needing authentication"
--   "O'\231Z\224\149\148\246\203[}\210\203\b\200\207"
mac
  :: BS.ByteString -- ^ 256-bit one-time key
  -> BS.ByteString -- ^ arbitrary-length message
  -> BS.ByteString -- ^ 128-bit message authentication code
mac key@(BI.PS _ _ kl) msg
    | kl /= 32  = error "ppad-poly1305 (mac): invalid key"
    | otherwise =
        let (clamp . roll -> r, roll -> s) = BS.splitAt 16 key

            loop !acc !bs = case BS.splitAt 16 bs of
              (chunk@(BI.PS _ _ l), etc)
                | l == 0 -> BS.take 16 (unroll16 (acc + s))
                | otherwise ->
                    let !n = roll chunk .|. (0x01 .<<. (8 * l))
                        !nacc = r * (acc + n) `rem` p
                    in  loop nacc etc

        in  loop 0 msg
  where
    p = 1361129467683753853853498429727072845819 -- (1 << 130) - 5


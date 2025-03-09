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

clamp :: Integer -> Integer
clamp r = r .&. 0x0ffffffc0ffffffc0ffffffc0fffffff
{-# INLINE clamp #-}

-- | Produce a Poly1305 MAC for the provided message, given the provided
--   key.
--
--   Per RFC8439, the key must be exactly 256 bits in length. Providing
--   an invalid key will cause the function to throw an ErrorCall
--   exception.
--
--   >>> mac "don't tell anyone my secret key!" "a message needing authentication"
--   ";]\a\USf\132A\156\b\171-_\162-\201R"
mac
  :: BS.ByteString -- ^ key
  -> BS.ByteString -- ^ message
  -> BS.ByteString -- ^ message authentication code
mac key@(BI.PS _ _ kl) msg
    | kl /= 32  = error "ppad-poly1305 (mac): invalid key"
    | otherwise =
        let (clamp . roll -> r, roll -> s) = BS.splitAt 16 key

            loop !acc !bs = case BS.splitAt 16 bs of
              (chunk@(BI.PS _ _ l), etc)
                | l == 0 -> BS.take 16 (unroll (acc + s))
                | otherwise ->
                    let !n = roll chunk .|. (0x01 .<<. (8 * l))
                        !nacc = r * (acc + n) `rem` p
                    in  loop nacc etc

        in  loop 0 msg
  where
    p = 1361129467683753853853498429727072845819 -- (1 << 130) - 5



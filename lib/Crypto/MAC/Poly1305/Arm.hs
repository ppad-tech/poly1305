{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Crypto.MAC.Poly1305.Arm
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- ARM acceleration for the Poly1305 MAC.

module Crypto.MAC.Poly1305.Arm (
    poly1305_arm_available
  , mac
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- ffi ------------------------------------------------------------------------

foreign import ccall unsafe "poly1305_mac_arm"
  c_poly1305_mac
    :: Ptr Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import ccall unsafe "poly1305_arm_available"
  c_poly1305_arm_available :: IO CInt

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- api ------------------------------------------------------------------------

-- | Are ARM extensions available?
poly1305_arm_available :: Bool
poly1305_arm_available =
  unsafeDupablePerformIO c_poly1305_arm_available /= 0
{-# NOINLINE poly1305_arm_available #-}

-- | Compute a Poly1305 MAC over the message using the given (already-
--   validated 32-byte) key.
mac :: BS.ByteString -> BS.ByteString -> BS.ByteString
mac (BI.PS kfp koff _) (BI.PS mfp moff mlen) =
  BI.unsafeCreate 16 $ \dst ->
    withForeignPtr kfp $ \kp0 ->
    withForeignPtr mfp $ \mp0 ->
      c_poly1305_mac (kp0 `plusPtr` koff)
                     (mp0 `plusPtr` moff)
                     (fi mlen)
                     dst

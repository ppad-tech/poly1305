{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.ByteString as BS
import Weigh

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith mac

mac :: Weigh ()
mac =
  let !key = BS.replicate 32 0x88
      !bs0 = BS.replicate 16 0
      !bs1 = BS.replicate 64 0
      !bs2 = BS.replicate 128 0
      !bs3 = BS.replicate 1024 0
  in  wgroup "mac" $ do
        func "mac (16B   input)" (Poly1305.mac key) bs0
        func "mac (64B   input)" (Poly1305.mac key) bs1
        func "mac (128B  input)" (Poly1305.mac key) bs2
        func "mac (1024B input)" (Poly1305.mac key) bs3

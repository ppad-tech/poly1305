{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.ByteString.Base16 as B16
import Test.Tasty
import qualified Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain $ testGroup "ppad-poly1305" [
    mac
  , mac0
  ]

mac :: TestTree
mac = H.testCase "mac" $ do
  let Just key = B16.decode
        "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
      msg = "Cryptographic Forum Research Group"

      Just e = B16.decode "a8061dc1305136c6c22b8baf0c0127a9"

      o = Poly1305.mac key msg
  H.assertEqual mempty e o

mac0 :: TestTree
mac0 = H.testCase "mac (A.3 #1)" $ do
  let Just key = B16.decode
        "0000000000000000000000000000000000000000000000000000000000000000"
      Just msg = B16.decode
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      Just tag = B16.decode
        "00000000000000000000000000000000"
      out = Poly1305.mac key msg
  H.assertEqual mempty tag out


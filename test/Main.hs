{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.ByteString.Base16 as B16
import Test.Tasty
import qualified Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain $ testGroup "ppad-poly1305" [
    mac
  , mac1
  , mac2
  , mac3
  , mac4
  , mac5
  , mac6
  , mac7
  , mac8
  , mac9
  , mac10
  , mac11
  ]

mac :: TestTree
mac = H.testCase "mac" $ do
  let Just key = B16.decode
        "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
      msg = "Cryptographic Forum Research Group"

      Just e = B16.decode "a8061dc1305136c6c22b8baf0c0127a9"

      Just o = Poly1305.mac key msg
  H.assertEqual mempty e o

mac1 :: TestTree
mac1 = H.testCase "mac (A.3 #1)" $ do
  let Just key = B16.decode
        "0000000000000000000000000000000000000000000000000000000000000000"
      Just msg = B16.decode
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      Just tag = B16.decode
        "00000000000000000000000000000000"
      Just out = Poly1305.mac key msg
  H.assertEqual mempty tag out

mac2 :: TestTree
mac2 = H.testCase "mac (A.3 #2)" $ do
  let Just key = B16.decode
        "0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e"
      Just msg = B16.decode
        "416e79207375626d697373696f6e20746f20746865204945544620696e74656e6465642062792074686520436f6e7472696275746f7220666f72207075626c69636174696f6e20617320616c6c206f722070617274206f6620616e204945544620496e7465726e65742d4472616674206f722052464320616e6420616e792073746174656d656e74206d6164652077697468696e2074686520636f6e74657874206f6620616e204945544620616374697669747920697320636f6e7369646572656420616e20224945544620436f6e747269627574696f6e222e20537563682073746174656d656e747320696e636c756465206f72616c2073746174656d656e747320696e20494554462073657373696f6e732c2061732077656c6c206173207772697474656e20616e6420656c656374726f6e696320636f6d6d756e69636174696f6e73206d61646520617420616e792074696d65206f7220706c6163652c207768696368206172652061646472657373656420746f"
      Just tag = B16.decode
        "36e5f6b5c5e06070f0efca96227a863e"
      Just out = Poly1305.mac key msg
  H.assertEqual mempty tag out

mac3 :: TestTree
mac3 = H.testCase "mac (A.3 #3)" $ do
  let Just key = B16.decode
        "36e5f6b5c5e06070f0efca96227a863e00000000000000000000000000000000"
      Just msg = B16.decode
        "416e79207375626d697373696f6e20746f20746865204945544620696e74656e6465642062792074686520436f6e7472696275746f7220666f72207075626c69636174696f6e20617320616c6c206f722070617274206f6620616e204945544620496e7465726e65742d4472616674206f722052464320616e6420616e792073746174656d656e74206d6164652077697468696e2074686520636f6e74657874206f6620616e204945544620616374697669747920697320636f6e7369646572656420616e20224945544620436f6e747269627574696f6e222e20537563682073746174656d656e747320696e636c756465206f72616c2073746174656d656e747320696e20494554462073657373696f6e732c2061732077656c6c206173207772697474656e20616e6420656c656374726f6e696320636f6d6d756e69636174696f6e73206d61646520617420616e792074696d65206f7220706c6163652c207768696368206172652061646472657373656420746f"
      Just tag = B16.decode
        "f3477e7cd95417af89a6b8794c310cf0"
      Just out = Poly1305.mac key msg
  H.assertEqual mempty tag out

mac4 :: TestTree
mac4 = H.testCase "mac (A.3 #4)" $ do
  let Just key = B16.decode
        "1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0"
      Just msg = B16.decode
        "2754776173206272696c6c69672c20616e642074686520736c6974687920746f7665730a446964206779726520616e642067696d626c6520696e2074686520776162653a0a416c6c206d696d737920776572652074686520626f726f676f7665732c0a416e6420746865206d6f6d65207261746873206f757467726162652e"
      Just tag = B16.decode
        "4541669a7eaaee61e708dc7cbcc5eb62"
      Just out = Poly1305.mac key msg
  H.assertEqual mempty tag out

mac5 :: TestTree
mac5 = H.testCase "mac (A.3 #5)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "02000000000000000000000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "ffffffffffffffffffffffffffffffff"
      Just tag = B16.decode
        "03000000000000000000000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac6 :: TestTree
mac6 = H.testCase "mac (A.3 #6)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "02000000000000000000000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "ffffffffffffffffffffffffffffffff"
      Just msg = B16.decode
        "02000000000000000000000000000000"
      Just tag = B16.decode
        "03000000000000000000000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac7 :: TestTree
mac7 = H.testCase "mac (A.3 #7)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "01000000000000000000000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "fffffffffffffffffffffffffffffffff0ffffffffffffffffffffffffffffff11000000000000000000000000000000"
      Just tag = B16.decode
        "05000000000000000000000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac8 :: TestTree
mac8 = H.testCase "mac (A.3 #8)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "01000000000000000000000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "fffffffffffffffffffffffffffffffffbfefefefefefefefefefefefefefefe01010101010101010101010101010101"
      Just tag = B16.decode
        "00000000000000000000000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac9 :: TestTree
mac9 = H.testCase "mac (A.3 #9)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "02000000000000000000000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "fdffffffffffffffffffffffffffffff"
      Just tag = B16.decode
        "faffffffffffffffffffffffffffffff"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac10 :: TestTree
mac10 = H.testCase "mac (A.3 #10)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "01000000000000000400000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "e33594d7505e43b900000000000000003394d7505e4379cd01000000000000000000000000000000000000000000000001000000000000000000000000000000"
      Just tag = B16.decode
        "14000000000000005500000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out

mac11 :: TestTree
mac11 = H.testCase "mac (A.3 #11)" $ do
  let Just (Poly1305._roll -> r) = B16.decode $
        "01000000000000000400000000000000"
      Just (Poly1305._roll -> s) = B16.decode $
        "00000000000000000000000000000000"
      Just msg = B16.decode
        "e33594d7505e43b900000000000000003394d7505e4379cd010000000000000000000000000000000000000000000000"
      Just tag = B16.decode
        "13000000000000000000000000000000"
      out = Poly1305._poly1305_loop r s msg
  H.assertEqual mempty tag out


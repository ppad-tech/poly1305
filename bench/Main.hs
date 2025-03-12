{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromJust)

main :: IO ()
main = defaultMain [
    suite
  ]

msg :: BS.ByteString
msg = fromJust . B16.decode $
  "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e"

key_small :: BS.ByteString
key_small = fromJust . B16.decode $
  "0000000000000000000000000000000000000000000000000000000000000003"

key_big :: BS.ByteString
key_big = fromJust . B16.decode $
  "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff3"

suite :: Benchmark
suite =
  bgroup "ppad-poly1305" [
    bench "mac (small key)" $ nf (Poly1305.mac key_small) msg
  , bench "mac (big key)" $ nf (Poly1305.mac key_big) msg
  ]


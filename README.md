# poly1305

[![](https://img.shields.io/hackage/v/ppad-poly1305?color=blue)](https://hackage.haskell.org/package/ppad-poly1305)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-poly1305-lightblue)](https://docs.ppad.tech/poly1305)

A fast Haskell implementation of the Poly1305 message authentication
code as specified by [RFC8439][8439].

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Crypto.MAC.Poly1305 as Poly1305
  >
  > -- produce a MAC for a message using a secret one-time key
  > let key = "i'll never use this key again!!!"
  > let msg = "i am a message that is in need of authentication"
  > Poly1305.mac key msg
  Just "\247\247\GSZ^\140\168\r\177\197\242\182b#\210g"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/poly1305][hadoc].

## Performance

The aim is best-in-class performance. Current benchmark figures on the
simple "sunscreen input" from RFC8439 on an M4 Silicon MacBook Air,
where we avail of hardware acceleration via ARM NEON intrinsics, look
like (use `cabal bench` to run the benchmark suite):

```
  benchmarking ppad-poly1305/mac (big key)
  time                 67.61 ns   (67.41 ns .. 67.86 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 67.67 ns   (67.50 ns .. 67.96 ns)
  std dev              742.4 ps   (489.7 ps .. 1.169 ns)
```

On longer inputs the NEON 4-way parallel kernel kicks in, with
correspondingly better throughput:

```
  benchmarking ppad-poly1305/mac (1024B msg)
  time                 224.9 ns   (224.5 ns .. 225.5 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 224.9 ns   (224.6 ns .. 225.5 ns)
  std dev              1.300 ns   (577.5 ps .. 2.512 ns)

  benchmarking ppad-poly1305/mac (4096B msg)
  time                 827.1 ns   (824.4 ns .. 831.0 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 825.1 ns   (824.3 ns .. 826.7 ns)
  std dev              3.649 ns   (2.093 ns .. 6.829 ns)
```

You should compile with the 'llvm' flag for maximum performance.

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

The Poly1305 MAC function and its internals pass all official
test vectors in RFC8439, and the downstream AEAD-ChaCha20-Poly1305
implementation in [ppad-aead](https://github.com/ppad-tech/aead) passes
all the [Project Wycheproof vectors][wyche].

Fixed-width words and constant-time primitives are supplied by
[ppad-fixed][fixed]. Criterion benchmarks provide sanity checks of
constant-time execution:

```
  benchmarking ppad-poly1305/mac (small key)
  time                 67.91 ns   (67.56 ns .. 68.30 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 67.60 ns   (67.47 ns .. 67.77 ns)
  std dev              505.8 ps   (380.4 ps .. 754.9 ps)

  benchmarking ppad-poly1305/mac (mid key)
  time                 67.72 ns   (67.52 ns .. 68.03 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 68.07 ns   (67.72 ns .. 69.24 ns)
  std dev              1.978 ns   (619.1 ps .. 4.006 ns)

  benchmarking ppad-poly1305/mac (big key)
  time                 67.61 ns   (67.41 ns .. 67.86 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 67.67 ns   (67.50 ns .. 67.96 ns)
  std dev              742.4 ps   (489.7 ps .. 1.169 ns)
```

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-poly1305
```

to get a REPL for the main library.

[8439]: https://datatracker.ietf.org/doc/html/rfc8439
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/poly1305
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[wyche]: https://github.com/C2SP/wycheproof
[fixed]: https://github.com/ppad-tech/fixed

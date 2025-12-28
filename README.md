# poly1305

[![](https://img.shields.io/hackage/v/ppad-poly1305?color=blue)](https://hackage.haskell.org/package/ppad-poly1305)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-poly1305-lightblue)](https://docs.ppad.tech/poly1305)

A pure Haskell implementation of the Poly1305 message authentication
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

The aim is best-in-class performance for pure Haskell code.

Current benchmark figures on the simple "sunscreen input" from RFC8439
on an M4 Silicon MacBook Air look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking ppad-poly1305/mac (big key)
  time                 125.1 ns   (124.9 ns .. 125.4 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 125.4 ns   (125.0 ns .. 126.2 ns)
  std dev              1.530 ns   (216.3 ps .. 2.693 ns)
```

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
  time                 125.1 ns   (124.9 ns .. 125.4 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 125.1 ns   (125.0 ns .. 125.4 ns)
  std dev              524.6 ps   (180.6 ps .. 1.132 ns)

  benchmarking ppad-poly1305/mac (mid key)
  time                 125.2 ns   (124.9 ns .. 125.4 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 125.1 ns   (125.1 ns .. 125.3 ns)
  std dev              441.3 ps   (195.0 ps .. 755.1 ps)

  benchmarking ppad-poly1305/mac (big key)
  time                 125.1 ns   (124.9 ns .. 125.4 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 125.4 ns   (125.0 ns .. 126.2 ns)
  std dev              1.530 ns   (216.3 ps .. 2.693 ns)
  variance introduced by outliers: 12% (moderately inflated)
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

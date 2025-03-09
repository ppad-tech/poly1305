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
  "\247\247\GSZ^\140\168\r\177\197\242\182b#\210g"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/poly1305][hadoc].

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on the simple "sunscreen input" from RFC8439
on my mid-2020 MacBook Air look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking ppad-poly1305/mac
  time                 9.880 μs   (9.596 μs .. 10.11 μs)
                       0.995 R²   (0.993 R² .. 0.997 R²)
  mean                 9.663 μs   (9.471 μs .. 9.879 μs)
  std dev              715.4 ns   (629.7 ns .. 828.0 ns)
  variance introduced by outliers: 77% (severely inflated)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

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

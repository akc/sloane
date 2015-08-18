# sloane [![Build Status](https://travis-ci.org/akc/sloane.svg)](https://travis-ci.org/akc/sloane)

A command line interface to
[The On-Line Encyclopedia of Integer Sequences](http://oeis.org).

![demo](demo.gif)

## Install

If using the [nix](https://nixos.org/nix/) package manager:

```
$ nix-env -f "<nixpkgs>" -iA haskellPackages.sloane
```

Otherwise, use [cabal](https://www.haskell.org/cabal/):

```
$ cabal install sloane
```

## Usage

See the [man page](sloane.md).

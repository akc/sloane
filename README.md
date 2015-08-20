# sloane [![Build Status](https://travis-ci.org/akc/sloane.svg)](https://travis-ci.org/akc/sloane)

A command line interface to the
[On-Line Encyclopedia of Integer Sequences](http://oeis.org).

![demo](https://github.com/akc/sloane/raw/master/demo.gif)

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

See the [man page](https://github.com/akc/sloane/blob/master/sloane.md).

## Issues

Have you found a bug? Want to contribute to `sloane`? Please open an issue
at <https://github.com/akc/sloane/issues>.

## How to cite

```
@misc{sloane,
  author = "Anders Claesson",
  title  = "sloane: A command line interface to the OEIS",
  year   =  2015,
  howpublished = "\url{http://akc.is/src/sloane}"
}
```

## License

BSD-3: see the
[LICENSE](https://github.com/akc/sloane/blob/master/LICENSE) file.

ghci-ng - Next generation GHCi
=====

This is GHCi plus extra goodies. It is a direct codebase copy from the
GHC codebase of GHCi with modifications to be buildable as a Hackage
package.

## Installing

From source:

    $ git clone git@github.com:chrisdone/ghci-ng.git
    $ cd ghci-ng
    $ cabal install

From Hackage:

    $ cabal update
    $ cabal install ghci-ng

## Using with cabal repl

Run `cabal repl` in the following way:

    $ cabal repl --with-ghc=ghci-ng

## Supported GHC versions

This project should ideally support the current major GHC version and
the past major GHC version:

* [GHC 7.8](https://github.com/ghc/ghc/releases/tag/ghc-7.8.2-release)

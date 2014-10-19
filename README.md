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

## Check it

It should say `GHCi-ng` on the welcome message:

    $ ghci-ng
    GHCi-ng, version 7.8.2 [NG/0.0]: http://www.haskell.org/ghc/  :? for help

You're now using the next generation GHCi!

## Using with cabal repl

Run `cabal repl` in the following way:

    $ cabal repl --with-ghc=ghci-ng

## Using with haskell-mode

Using with haskell-mode is very easy, it's a drop-in replacement for
the normal use of GHCi.

If you use the `cabal-repl` process type, use:

``` lisp
(setq haskell-process-args-cabal-repl
      '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
```

Plain GHCi process type:

``` lisp
(setq haskell-process-path-ghci "ghci-ng")
```

## Supported GHC versions

This project should ideally support the current major GHC version and
the past major GHC version:

* [GHC 7.8](https://github.com/ghc/ghc/releases/tag/ghc-7.8.2-release)

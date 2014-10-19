ghci-ng - Next generation GHCi
=====

This is GHCi plus extra goodies. It is a direct codebase copy from the
GHC codebase of GHCi with modifications to be buildable as a Hackage
package.

## Features

The features additional to normal GHCi function in the latest GHC
(7.8.3) release are:

* The `:set +c` command: collect information about modules after
  they've been loaded, and remember it between loads (including failed
  ones).¹

* The `:type-at` command: show the type at the given position in the
  module. Example:

        *X> :type-at X.hs 6 6 6 7 f
        Int -> Int

  This can be useful to get the type of a pattern variable or an
  arbitrary selected expression.

* The `:loc-at` command: get the location of the thing at the given
  position in the module. Example:

        *X> :loc-at X.hs 6 14 6 16 mu
        X.hs:8:7-8:9

  This is useful for goto-definition features of editors and IDEs.

¹Currently, this will re-collect info every time, because I'm not
(yet) doing a last-modified check on the interfaces of the
modules. Once that's done, only the current module's type/location
info will need to be regenerated.

## Installing

From source:

    $ git clone git@github.com:chrisdone/ghci-ng.git
    $ cd ghci-ng
    $ cabal install

This version is not available on Hackage yet.

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

Commands that use GHCi-ng:

* `M-x haskell-mode-show-type-at`
* `M-x haskell-mode-goto-loc`

Make sure to run `:set +c` in GHCi to enable collection of info, then
load some modules and the above commands will work.

## Supported GHC versions

This project should ideally support the current major GHC version and
the past major GHC version:

* [GHC 7.8](https://github.com/ghc/ghc/releases/tag/ghc-7.8.2-release)

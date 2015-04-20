ghci-ng - Next generation GHCi
=====

This is GHCi plus extra goodies. It is a direct codebase copy from the
GHC codebase of GHCi with modifications to be buildable as a Hackage
package.

## Features

The features additional to normal GHCi function in the latest GHC
(7.8.4) release are:

* The `:set +c` command: collect information about modules after
  they've been loaded, and remember it between loads (including failed
  ones). I recommend adding this line to your `~/.ghci` file.

* The `:type-at` command (requires `+c`): show the type at the given position in the
  module. Example:

  ``` haskell
  *X> :type-at X.hs 6 6 6 7 f
  Int -> Int
  ```

  This can be useful to get the type of a pattern variable or an
  arbitrary selected expression.

  The parameters are:

        <filename> <line> <col> <end-line> <end-col> <text>

  `text` is used for when the span is out of date, i.e. the file
  changed and the code has moved, text can be used to confirm the
  contents of the span matches, and to fallback to a general :t-like
  lookup.

* The `:loc-at` command (requires `+c`): get the location of the thing at the given
  position in the module. Example:

  ``` haskell
  *X> :loc-at X.hs 6 14 6 16 mu
  X.hs:(8,7)-(8,9)
  ```

  This is useful for goto-definition features of editors and IDEs.

  Parameters are the same as for `:type-at`.

* The `:uses` command (requires `+c`): gets all module-local uses of
  the thing at the given position in the module. Example:

  ``` haskell
  > :uses /home/chris/Projects/ghci-ng/ghc/GhciFind.hs 53 66 53 70 name
  /home/chris/Projects/ghci-ng/ghc/GhciFind.hs:(46,25)-(46,29)
  /home/chris/Projects/ghci-ng/ghc/GhciFind.hs:(47,37)-(47,41)
  /home/chris/Projects/ghci-ng/ghc/GhciFind.hs:(53,66)-(53,70)
  /home/chris/Projects/ghci-ng/ghc/GhciFind.hs:(57,62)-(57,66)
  ```

  This is useful for highlighting and navigating all uses of an
  identifier in editors and IDEs.

  Parameters are the same as for `:type-at`.

* The `:all-types` command (requires `+c`): list *all* types in the project:
  expressions, bindings top-level and local. Sort of like `:browse` on
  steroids.

  ``` haskell
  > :all-types
  ghc/GhciTypes.hs:(38,13)-(38,24): Maybe Id
  ghc/GhciTypes.hs:(45,10)-(45,29): Outputable SpanInfo
  ghc/GhciTypes.hs:(45,10)-(45,29): (Rational -> SpanInfo -> SDoc) -> Outputable SpanInfo
  ```

Columns start at 1 instead of zero, because this is how GHC now
outputs its column numbers.

## Installing

### Requirements

`GHCi-ng` requires development version of `tinfo` library. For example, Debian
users can install it from Debian packages using following command:

    $ apt-get install libtinfo-dev

### Installing from source:

    $ git clone https://github.com/chrisdone/ghci-ng.git
    $ cabal install ghci-ng/

This version is not available on Hackage yet.

(Hackage flat out rejects the `-dynamic` flag which is required for
GHCi to work properly.)

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

**Make sure to run `:set +c` in GHCi to enable collection of info, then
load some modules and the above commands will work.**

### Go to definition

``` lisp
(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
```

![http://i.imgur.com/wq4mU5L.gif](http://i.imgur.com/wq4mU5L.gif)

### Type info

``` lisp
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
```

![http://i.imgur.com/cu2YGfQ.gif](http://i.imgur.com/cu2YGfQ.gif)

### Find uses

Choose a keybinding that suites you best. I am currently using `C-?`.

``` lisp
(define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
```

![http://i.imgur.com/Ue7m5pm.gif](http://i.imgur.com/Ue7m5pm.gif)

## Use with Vim

Patches welcome.

## Use with SublimeText

Patches welcome.

## Supported GHC versions

This project should ideally support the current major GHC version and
the past major GHC version:

* [GHC 7.8.4](https://github.com/ghc/ghc/releases/tag/ghc-7.8.4-release)
* [GHC 7.8.3](https://github.com/ghc/ghc/releases/tag/ghc-7.8.3-release)
* [GHC 7.8.2](https://github.com/ghc/ghc/releases/tag/ghc-7.8.2-release)

# <img src="https://github.com/commercialhaskell/intero/raw/master/images/intero.svg" height=25> intero [![Build Status](https://travis-ci.org/commercialhaskell/intero.png)](https://travis-ci.org/commercialhaskell/intero) [![MELPA](https://melpa.org/packages/intero-badge.svg)](https://melpa.org/#/intero) [![MELPA Stable](https://stable.melpa.org/packages/intero-badge.svg)](https://stable.melpa.org/#/intero)

Complete interactive development program for Haskell

## Intero for Emacs

Please see
[the homepage for Intero for Emacs](http://commercialhaskell.github.io/intero).

#### Default key bindings

Key binding | Description
--- | ---
`M-.` | Jump to definition
`C-c C-i` | Show information of identifier at point
`C-c C-t` | Show the type of thing at point, or the selection
`C-u C-c C-t` | Insert a type signature for the thing at point
`C-c C-l` | Load this module in the REPL
`C-c C-c` | Evaluate the selected region in the REPL
`C-c C-r` | Apply suggestions from GHC
`C-c C-k` | Clear REPL
`C-c C-z` | Switch to and from the REPL

## Whitelisting/blacklisting projects

Typically Intero will enable for all projects, and for files
without a stack.yaml, it will assume the "global" project. Some users
prefer to enable Intero selectively. See below how to do that.

Find this line in your Emacs configuration and remove it:

``` lisp
(add-hook 'haskell-mode-hook 'intero-mode)
```

To whitelist specific directories (and ignore everything else), use:

``` lisp
(setq intero-whitelist '("/work/directories/" "/my/directories/"))
(add-hook 'haskell-mode-hook 'intero-mode-whitelist)
```

To blacklist specific directories (and allow everything else), use:

``` lisp
(setq intero-blacklist '("/path/to/bad/project" "/path/to/ignore/me"))
(add-hook 'haskell-mode-hook 'intero-mode-blacklist)
```

## Intero for IDE writers

Please see
[the TOOLING.md file for how to use Intero to integrate your own editor.](https://github.com/commercialhaskell/intero/blob/master/TOOLING.md).

## Issues

Issues are split into low/medium/high priorities which dictates which
ones will be implemented first.

* [High priority issues](https://github.com/commercialhaskell/intero/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22priority%3A+high%22+)
* [Medium priority issues](https://github.com/commercialhaskell/intero/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22priority%3A+medium%22)
* [Low priority issues](https://github.com/commercialhaskell/intero/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22priority%3A+low%22)
* [Unprioritized issues](https://github.com/commercialhaskell/intero/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+-label%3A%22priority%3A+low%22++-label%3A%22priority%3A+medium%22++-label%3A%22priority%3A+high%22+)

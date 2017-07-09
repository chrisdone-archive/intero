# <img src="https://github.com/commercialhaskell/intero/raw/master/images/intero.svg" height=25> intero [![Build Status](https://travis-ci.org/commercialhaskell/intero.png)](https://travis-ci.org/commercialhaskell/intero) [![MELPA](https://melpa.org/packages/intero-badge.svg)](https://melpa.org/#/intero) [![MELPA Stable](https://stable.melpa.org/packages/intero-badge.svg)](https://stable.melpa.org/#/intero) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/commercialhaskell/path?svg=true)](https://ci.appveyor.com/project/commercialhaskell/intero)

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

## Enabling intero

To enable `intero` in all `haskell-mode` buffers by default, enable
`intero-global-mode`, by using `M-x customize` or by adding
`(intero-global-mode 1)` to your Emacs start-up files.

Intero will then activate for all projects, and for files without a
stack.yaml, it will assume the "global" project. If you want to use an
alternate stack yaml configuration file (for example, when developing
for multiple GHC versions), use `M-x intero-stack-yaml` to switch
file. When switching configuration, you will asked whether you want to
preserve this choice across emacs sessions for the given project.

## Whitelisting/blacklisting projects

Some users prefer to enable Intero selectively. The custom variables
`intero-blacklist` and `intero-whitelist` are provided for this
purpose, and are honoured by `intero-global-mode`:

If the parent directory of a Haskell file is listed in
`intero-blacklist`, then `intero` will not be enabled for that file,
unless a parent directory of that file is also listed in
`intero-whitelist`. In other words, whitelist entries take
precedence. You can therefore blacklist `/` to disable `intero` in all
projects unless they are whitelisted.


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

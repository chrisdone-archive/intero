# intero [![Build Status](https://travis-ci.org/chrisdone/intero.svg)](https://travis-ci.org/chrisdone/intero) [![Build status](https://ci.appveyor.com/api/projects/status/23bdffi0bmycxn50?svg=true)](https://ci.appveyor.com/project/chrisdone/intero)

Complete interactive development program for Haskell

# Intero for your favourite editors

There are various editor integrations for the Intero binary, with the
Emacs one being the officially supported one:

* [Emacs](https://github.com/chrisdone/intero/blob/master/EMACS.md) [officially supported]
* [Neovim](https://github.com/parsonsmatt/intero-neovim)
* [Haskero](https://gitlab.com/vannnns/haskero) (Visual Studio Code)
* [Haskelly](https://github.com/haskelly-dev/Haskelly) (Visual Studio Code)
* [SublimeText](https://github.com/dariusf/sublime-intero)
* [IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)

If you would like to build support into a new editor or IDE, please see
[TOOLING.md](https://github.com/commercialhaskell/intero/blob/master/TOOLING.md).

## Install requirements

Make sure you have `libtinfo` installed:

OS | Package
--- | ---
Arch Linux | `ncurses`
Debian derivatives | `libtinfo-dev`
Fedora derivatives | `ncurses-devel`

(People on other platforms please contribute the equivalent
dependency.)

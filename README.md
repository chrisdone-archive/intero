# <img src="https://github.com/commercialhaskell/intero/raw/master/images/intero.svg" height=25> intero [![Build Status](https://travis-ci.org/commercialhaskell/intero.png)](https://travis-ci.org/commercialhaskell/intero)

Complete interactive development program for Haskell

## Supported GHC versions

Intero been built and tested on the following GHC versions:

* GHC 8.0.1
* GHC 7.10.3
* GHC 7.10.2
* GHC 7.8.4

## Features

It's basically GHCi plus extra features. Those are:

* [Find uses of an identifier in a module.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L118)
* [Find definition of an identifier in a module.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L143)
* [Show the type of an expression or identifier](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L82).
* [List all types of all expressions of all modules loaded.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L98)
* [Completion of identifiers within a module's scope.](https://github.com/commercialhaskell/intero/blob/bbd71951edb89f06a939910024f85cc44c11c16e/src/test/Main.hs#L242)

Probably more to come.

## Requirements

The following dependencies are necessary:

* The `tinfo` and `ncurses` library.

  * Ubuntu and Debian users can install it using the following
    command:

          $ apt-get install libtinfo-dev
          $ apt-get install libncurses5-dev

## Installing

Use `stack build` (not `install`) for each of your package sets. Each
LTS or nightly should have a separate `stack build`. **If you use**
`stack install` **you will run into incompatibility issues
between package sets.**

Standard:

    $ stack build intero

From source:

    $ git clone https://github.com/commercialhaskell/intero.git
    $ cd intero
    $ stack build intero

## Running

To run it plainly use:

    $ stack exec intero

You'll have to run `stack build intero` within each separate LTS
version you use, this ensures that the intero you launch correctly
matches the GHC version that you're working with.

To load up your stack project use:

    $ stack ghci --with-ghc intero

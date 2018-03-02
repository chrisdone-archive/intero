# Backend features

It's basically GHCi plus extra features. Those are:

* [Find uses of an identifier in a module.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L118)
* [Find definition of an identifier in a module.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L143)
* [Show the type of an expression or identifier](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L82).
* [List all types of all expressions of all modules loaded.](https://github.com/commercialhaskell/intero/blob/28609611c9f7c7d63370ce66e8ebb97676a8374e/src/test/Main.hs#L98)
* [Completion of identifiers within a module's scope.](https://github.com/commercialhaskell/intero/blob/bbd71951edb89f06a939910024f85cc44c11c16e/src/test/Main.hs#L242)

# Installing

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

If your project's `stack.yaml` has `extra-deps` that include upgraded versions of any of the packages that GHC depends on, then intero will not build there. Instead, build intero in a different directory:

    $ pushd /tmp
    $ stack --resolver lts-10.7 build --copy-compiler-tool intero
    $ popd

# Running

To run it plainly use:

    $ stack exec intero

You'll have to run `stack build intero` within each separate LTS
version you use, this ensures that the intero you launch correctly
matches the GHC version that you're working with.

To load up your stack project use:

    $ stack ghci --with-ghc intero

Or

    $ cabal repl --with-ghc intero

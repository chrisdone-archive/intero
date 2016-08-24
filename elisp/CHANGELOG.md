# June-July 2016

Intero was made public in the start of June. Here's a rundown of
the changes made since then:

* Now when the backend fails to start, it stops retrying when
  you're working until you kill the buffer.
* When the backend is starting and it fails due to missing
  dependencies, it automatically re-runs without passing
  `--no-build` to stack; leading to build all the dependencies and
  then starting. This leads to a nice workflow of adding a package
  to the `.cabal` file and hitting `M-x intero-restart`.
* Auto-completion of imports and pragmas.
* Company-mode integration is asynchronous now, so it doesn't lock
  up the editor.
* Removed hlint from next-checkers as it was bothering
  people. It's easy to re-enable
  [with standard flycheck](https://github.com/commercialhaskell/intero/issues/126#issuecomment-228612696)
  settings.
* Now you can switch targets (e.g. `M-x intero-targets`) using the
  multi-switch view,
  [like this](https://github.com/commercialhaskell/intero/issues/56#issuecomment-229305346). Saves
  you having to remember your targets and the syntax for
  specifying them.
* You can now launch the REPL with `C-u` prefix so that it pops up
  [an options list](https://github.com/commercialhaskell/intero/pull/131#issuecomment-229275383)
  on how to start the REPL.
* Fixed a bug in the warnings parser.
* Added `intero-toggle-debug`
  ([#79](https://github.com/commercialhaskell/intero/issues/79),
  [#151](https://github.com/commercialhaskell/intero/pull/151)),
  good for debugging issues with Intero.
* Finally made a reliable way to save the current buffer for
  flycheck. This no longer interacts badly with magit or external
  changes to your files.
* Added `C-c C-z` to switch to and from the REPL.
* Added
  [a suggestions system](https://haskell-lang.org/intero#suggestions). When
  you hit `C-c C-r`, you get a list of suggestions that you can
  check and then apply with `C-c C-c`:

  * Automatically add extensions when GHC suggests them. Example:

         Can't make a derived instance of ‘Functor X’:
         You need DeriveFunctor to derive an instance for this class
         Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
         In the newtype declaration for ‘X’
  * Automatically remove redundant imports. Example:

         The import of ‘Control.Monad’ is redundant
           except perhaps to import instances from ‘Control.Monad’
         To import instances alone, use: import Control.Monad()... (intero)
  * Fix typos. Example:

         Not in scope: ‘putStrn’
         Perhaps you meant one of these:
           ‘putStr’ (imported from Prelude),
           ‘putStrLn’ (imported from Prelude)
  * Adding top-level type signatures. Example:

         Top-level binding with no type signature: main :: IO ()
  * Removing redundant class constraints. Example:

         Redundant constraints: (Arith var, Bitwise var)
  * And turning off warnings for name shadowing and type
    defaulting. (Checkbox is not checked by default.)
* And other miscellaneous bug fixes.

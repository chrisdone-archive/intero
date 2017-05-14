#!/bin/sh -e

ELDIR=$(dirname "$0")

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize)
  (package-refresh-contents))"

cd "$ELDIR"
echo '*** INSTALLING ELISP PREREQUISITES'
cask && echo OK

echo
echo '*** CHECKING ELISP BYTE-COMPILES CLEANLY'
cask emacs -Q --eval "(setq byte-compile-error-on-warn t)" \
     -batch -f batch-byte-compile ./*.el && echo OK

echo
echo '*** CHECKING ELISP FOR PACKAGING ISSUES'
cask emacs -Q --eval "$INIT_PACKAGE_EL" \
     -batch -l package-lint -f package-lint-batch-and-exit intero.el && echo OK


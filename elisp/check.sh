#!/bin/sh -e

ELDIR=$(dirname "$0")

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize)
  (package-refresh-contents))"

cd $ELDIR

cask emacs -Q --eval "(setq byte-compile-error-on-warn t)" \
     -batch -f batch-byte-compile *.el

cask emacs -q --eval "(require 'package-lint) $INIT_PACKAGE_EL" \
     -batch -f package-lint-batch-and-exit intero.el


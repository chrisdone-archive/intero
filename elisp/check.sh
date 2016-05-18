#!/bin/bash

for i in `ls *.el | grep -v case-split | sed 's/.el$//'`
 do rm -f $i.elc
 echo Compiling $i.el ...
 emacs -Q -L ../../dash -L ../../company-mode/ -L ../../haskell-mode -L ../../flycheck/ -L . --batch --eval "(byte-compile-disable-warning 'cl-functions)" -f batch-byte-compile $i.el

 done

 rm *.elc

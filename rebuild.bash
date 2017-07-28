#!/bin/bash -e
rm -f init.elc
emacs -Q --batch -l init.el -f batch-byte-compile init.el

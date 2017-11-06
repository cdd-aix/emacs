#!/bin/bash -e
[ -r cf-own-custom.el ] || cp cf-own-custom.el.default cf-own-custom.el
rm -f init.elc
emacs -Q --batch -l init.el -f batch-byte-compile init.el

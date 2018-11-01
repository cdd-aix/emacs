#!/bin/bash -eux
#[ -r cf-own-custom.el ] || cp cf-own-custom.el.default cf-own-custom.el
export LANG=en_US.UTF-8
zero=$(readlink -f "$0")
zerodir="${zero%/*}"
ebatch() {
    emacs -Q --batch -l "$zerodir/init.el" "$@"
}
main() {
    local userdir=~/p/emacs/elpa
    rm -f init.elc
    # ebatch -f batch-byte-compile "$zerodir/init.el"
    # userdir=$(ebatch --eval '(princ package-user-dir)' 2>/dev/null)
    ebatch --eval "(byte-recompile-directory \"$userdir\" 0 t)" --eval "(byte-compile-file \"$zerodir/init.el\")"
#    ebatch -f batch-byte-compile init.el
}
main "$@"

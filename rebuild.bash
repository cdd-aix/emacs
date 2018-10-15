#!/bin/bash -eux
#[ -r cf-own-custom.el ] || cp cf-own-custom.el.default cf-own-custom.el
zero=$(readlink -f "$0")
zerodir="${zero%/*}"
ebatch() {
    emacs -Q --batch -l "$zerodir/init.el" "$@"
}
main() {
    local userdir
    rm -f init.elc
    ebatch -f batch-byte-compile "$zerodir/init.el"
    userdir=$(ebatch --eval '(princ package-user-dir)' 2>/dev/null)
    ebatch -f batch-byte-recompile-directory "$userdir"
    ebatch -f batch-byte-compile init.el
}
main "$@"

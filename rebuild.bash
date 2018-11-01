#!/bin/bash -eux
#[ -r cf-own-custom.el ] || cp cf-own-custom.el.default cf-own-custom.el
export LANG=en_US.UTF-8
zero=$(readlink -f "$0")
zerodir="${zero%/*}"
ebatch() {
    emacs -Q --batch -l "$zerodir/init.el" "$@"
}
main() {
    local results
    rm -f "$zerodir/init.elc"
    results=$(time ebatch --eval "(byte-recompile-directory package-user-dir 0 t)" -f batch-byte-compile "$zerodir/init.el" 2>compile.log)
    ls -ld "$zerodir/init.elc"
}
main "$@"

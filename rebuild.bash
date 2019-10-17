#!/bin/bash -eux
#[ -r cf-own-custom.el ] || cp cf-own-custom.el.default cf-own-custom.el
export LANG=en_US.UTF-8
zero=$(readlink -f "$0")
zerodir="${zero%/*}"
ebatch() {
    emacs -Q --batch -l "$zerodir/init.el" "$@"
}
main() {
    local packageuserdir bytecompile
    # Side effect, installs missing packages
    rm -vf "$zerodir/init.elc"
    echo See fetch.log if packages are missing
    packageuserdir=$(ebatch --eval '(princ package-user-dir)' 2> fetch.log)
    echo package-user-dir "$packageuserdir"
    echo See compile.log if other weirdness
    bytecompile=$(ebatch --eval '(byte-recompile-directory package-user-dir 0 t)' -f batch-byte-compile "$zerodir/init.el" 2>compile.log)
    echo Byte Compile Results "$bytecompile"
    ebatch -f batch-byte-compile "$zerodir/init.el"
    # rm -f "$zerodir/init.elc"
    # if ! [ -d "$zerodir/elpa" ]; then
    # 	ebatch -f
    # results=$(time ebatch --eval "(byte-recompile-directory package-user-dir 0 t)" -f batch-byte-compile "$zerodir/init.el" 2>compile.log)
    # echo "$results"
    ls -ld "$zerodir/init.elc"
}
main "$@"

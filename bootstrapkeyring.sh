#!/bin/bash -eux
bootstrapkeyring() {
    if ! emacs -Q --batch -l bootstrapkeyring.el; then
	gpg --homedir elpa/gnupg --receive-keys 066DAFCB81E42C40
	emacs -Q --batch -l bootstrapkeyring.el
    fi
    [ -r "$1" ]
    exit 0
}

bootstrapkeyring "$@"

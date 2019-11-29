#!/bin/bash -eux
Prepare(){
    if ! add-apt-repository --help > /dev/null; then
	apt-get install -y software-properties-common
    fi

    add-apt-repository -y ppa:kelleyk/emacs
    if ! dpkg -l emacs25; then
	apt-get install -y emacs25
    else
	apt-get upgrade -y emacs25
    fi
    exit 0
}
Prepare "$@"

#!/bin/bash -eux
package=emacs27
Prepare(){
    if ! add-apt-repository --help > /dev/null; then
	apt-get install -y software-properties-common
    fi

    add-apt-repository -y ppa:kelleyk/emacs
    if ! dpkg -s "$package"; then
	apt-get install -y "$package"
    else
	apt-get upgrade -y "$package"
    fi
    exit 0
}
Prepare "$@"

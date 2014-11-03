#!/bin/sh

generate_package_names() {
    echo emacsconf
    echo misc
    if [ `uname` = 'Linux' ]; then
	    echo misc-linux
    elif [ `uname` = 'Darwin' ]; then
        echo misc-osx
    fi
}

if [ "$1" = 'prepare' ]; then
    IFS="$(printf '\n\t')"
    for package in `generate_package_names`; do
        if [ -e ${package}/Makefile ]; then
            make -C $package || exit 1
        fi
    done
    exit 0
fi

command -v stow 2>/dev/null 1>&2 || { echo "stow needed."; exit 0; }
cd $(dirname $0)

if [ "$1" = 'install' ]; then OP="-S"
elif [ "$1" = 'uninstall' ]; then OP="-D"
else 
    echo "install/uninstall?"
    exit 1
fi

generate_package_names | while read package; do
    echo "$1" $package
    stow --no-folding --ignore='^Makefile$' $OP $package
done

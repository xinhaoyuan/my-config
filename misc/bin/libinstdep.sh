#!/bin/bash

function join_by {
    local IFS="$1"
    shift
    echo "$*"
}

function install_dep {
    set -e
    local TEMP_DIR="$TEMP_DIR"
    local NAME=$1; shift
    local DEPS=$(join_by , "$@")
    [ -z "$TEMP_DIR" ] && TEMP_DIR=/tmp/dep-workspace-$$
    mkdir -m 0755 -p $TEMP_DIR
    mkdir -m 0755 -p $TEMP_DIR/$NAME/DEBIAN
    cat >$TEMP_DIR/$NAME/DEBIAN/control <<EOF
Package: $NAME
Maintainer: Xinhao Yuan
Version: 1.0
Architecture: all
Depends: $DEPS
Description: Dependency metapackage of $NAME.
EOF
    dpkg-deb -b $TEMP_DIR/$NAME
    sudo dpkg -i $TEMP_DIR/$NAME.deb || sudo apt-get --fix-broken install
    rm -rf $TEMP_DIR
}

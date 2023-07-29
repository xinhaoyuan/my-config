#!/bin/bash

set -e

. libinstdep.sh

DEP_ARRAY=(
    stow
    zsh
    tmux
    emacs
    php-cli
    python3-pip
    nano
    bc
    unzip
    socat
    wget
    curl
    htop
    pmount
    iptables
)

install_dep xy-base "${DEP_ARRAY[@]}"

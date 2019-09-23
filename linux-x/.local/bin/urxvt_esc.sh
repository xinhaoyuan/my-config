#!/bin/bash

ACT=$1; shift 1

case $ACT in
    set-font)
        printf '\033]710;xft:%s:size=10\007' "$1"
        ;;
    *)
        echo "No action performed."
        exit 1
        ;;
esac

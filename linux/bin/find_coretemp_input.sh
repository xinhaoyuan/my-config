#!/bin/sh

for p in /sys/class/hwmon/*; do
    name=$(cat "$p/name")
    if [ "$name" = "coretemp" ]; then
        for n in $p/*_label; do
            label=$(cat "$n")
            case "$label" in
                 Package*)
                     echo "${n%_label}_input"
                     break
                     ;;
            esac
        done
        break
    elif [ "$name" = "k10temp" ]; then
        # It is a relative reading with arbitary scale, but at least that is some number.
        echo "$p/temp1_input"
        break
    fi
done

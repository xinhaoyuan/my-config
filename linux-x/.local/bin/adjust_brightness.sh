#!/bin/bash

# Adjust the xrandr brightness for the current output of the mouse.

set -euo pipefail

eval "$(xdotool getmouselocation --shell --prefix MOUSE_)"

BRIGHTNESS_ADJUSTMENT="${1:-}"

readonly HEADER_PATTERN="(.*) (dis)?connected (primary )?([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+)"
readonly BRIGHTNESS_PATTERN="Brightness:[ \t]*([0-9.]+)"
readonly DELTA_PATTERN="^(-|\+)"

CURRENT=0
while read line;
do
    if [[ $line =~ $HEADER_PATTERN ]]; then
        if ((CURRENT)); then
            echo "No brightness information found in the current output" >&2
            break
        fi
        OUTPUT_NAME="${BASH_REMATCH[1]}"
        OUTPUT_WIDTH="${BASH_REMATCH[4]}"
        OUTPUT_HEIGHT="${BASH_REMATCH[5]}"
        OUTPUT_X="${BASH_REMATCH[6]}"
        OUTPUT_Y="${BASH_REMATCH[7]}"
        CURRENT=$(( OUTPUT_X <= MOUSE_X && MOUSE_X < OUTPUT_X + OUTPUT_WIDTH &&
                        OUTPUT_Y <= MOUSE_Y && MOUSE_Y < OUTPUT_Y + OUTPUT_HEIGHT ))
    elif ((CURRENT)) && [[ $line =~ $BRIGHTNESS_PATTERN ]]; then
        BRIGHTNESS=${BASH_REMATCH[1]}
        if [[ -z $BRIGHTNESS_ADJUSTMENT ]]; then
            echo "${BRIGHTNESS}"
        elif [[ $BRIGHTNESS_ADJUSTMENT =~ $DELTA_PATTERN ]]; then
            NEW_BRIGHTNESS="$(echo "x=$BRIGHTNESS $BRIGHTNESS_ADJUSTMENT; if (0>x) 0 else if (x>1) 1 else x" | bc)" 
            xrandr --output "${OUTPUT_NAME}" --brightness "${NEW_BRIGHTNESS}"
        else
            xrandr --output "${OUTPUT_NAME}" --brightness "${BRIGHTNESS_ADJUSTMENT}"
        fi
        break
    fi
done < <(xrandr --current --verbose)

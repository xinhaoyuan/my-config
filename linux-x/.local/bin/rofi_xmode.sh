#!/bin/bash

echo "INFO: $ROFI_INFO" >&2
if [[ $ROFI_INFO = "enter-mode "* ]]; then
    MODE="${ROFI_INFO#enter-mode }"
    ITEM=""
else
    MODE="${ROFI_INFO%% *}"
    ITEM="${ROFI_INFO#${MODE} }"
fi
echo "MODE[$MODE] ITEM[$ITEM]" >&2

action() {
    echo -e "$1\0info\x1f$MODE $2"
}

mode() {
    echo -e "$1\0info\x1fenter-mode $2"
}

case "$MODE" in
    "")
        case "$ITEM" in
            "")
                mode "Toggle xinput devices" "toggle-xinput"
                ;;
        esac
        ;;
    toggle-xinput)
        case "$ITEM" in
            "")
                IFS=$'\n'
                XINPUT_REGEX="↳ (.*[^ ]) *id=([0-9]*)"
                for line in `xinput list | sort`; do
                    if [[ $line =~ $XINPUT_REGEX ]]; then
                        NAME="${BASH_REMATCH[1]}"
                        ID="${BASH_REMATCH[2]}"
                        action "$NAME" "$ID"
                    fi
                done
                ;;
            *)
                ID="$ITEM"
                ENABLED_LINE=`xinput list-props "$ID" | grep -e "Device Enabled"`
                if [[ $ENABLED_LINE = *:*1 ]]; then
                    echo "Disabling xinput device $ID" >&2
                    coproc xinput disable "$ID" >/dev/null
                else
                    echo "Enabling xinput device $ID" >&2
                    coproc xinput enable "$ID" >/dev/null
                fi
                ;;
        esac
        ;;
esac

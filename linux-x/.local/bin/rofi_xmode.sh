#!/bin/bash

source rofi_script_lib.sh

rofi_script_entry() {
    local MODE="$1"
    local ACTION="$2"
    case "$MODE" in
        "")
            case "$ACTION" in
                "")
                    rofi_action "Toggle xinput devices" "enter-mode toggle-xinput"
                    ;;
            esac
            ;;
        toggle-xinput)
            if [[ -n "$ACTION" ]]; then
                local ID="$ACTION"
                local ENABLED_LINE=`xinput list-props "$ID" | grep -e "Device Enabled"`
                if [[ $ENABLED_LINE = *:*1 ]]; then
                    echo "Disabling xinput device $ID" >&2
                    xinput disable "$ID" >/dev/null
                else
                    echo "Enabling xinput device $ID" >&2
                    xinput enable "$ID" >/dev/null
                fi
            fi
            list_xinput_devices "$ACTION"
            ;;
    esac
}

list_xinput_devices() {
    local PREVIOUS_ID="$1"
    local IFS=$'\n'
    local XINPUT_REGEX="↳ (.*[^ ]) *id=([0-9]*)"
    local XINPUT_DISABLE_REGEX=".*This device is disabled.*"
    local -a DEVICE_NAME
    local -A DEVICE_ENABLED
    local -A DEVICE_ID
    for line in `xinput list --long`; do
        if [[ $line =~ $XINPUT_REGEX ]]; then
            NAME="${BASH_REMATCH[1]} (${BASH_REMATCH[2]})"
            ID="${BASH_REMATCH[2]}"
            DEVICE_NAME+=("$NAME")
            DEVICE_ID[$NAME]="$ID"
            DEVICE_ENABLED[$NAME]=1
        elif [[ $line = *"This device is disabled" ]]; then
            DEVICE_ENABLED[$NAME]=0
        fi
    done
    DEVICE_NAME=($(sort <<<"${DEVICE_NAME[*]}"))
    for NAME in "${DEVICE_NAME[@]}"; do
        local ATTR=""
        if [[ ${DEVICE_ID[$NAME]} = $PREVIOUS_ID ]]; then
            ATTR=selected
        fi
        if (( ${DEVICE_ENABLED[$NAME]} )); then
            rofi_action "$NAME" "${DEVICE_ID[$NAME]}" "$ATTR"
        else
            rofi_action "$NAME disabled" "${DEVICE_ID[$NAME]}" "$ATTR"
        fi
    done
}

#!/bin/bash

echo "INFO: $ROFI_INFO" >&2
if [[ $ROFI_INFO = "enter-mode "* ]]; then
    MODE="${ROFI_INFO#enter-mode }"
    ACTION=""
else
    MODE="${ROFI_INFO%% *}"
    ACTION="${ROFI_INFO#${MODE} }"
fi
echo "MODE[$MODE] ACTION[$ACTION]" >&2

declare -a _ROFI_ACTION_NAME
declare -a _ROFI_ACTION_INFO
declare -a _ROFI_ACTION_ACTIVE
declare -a _ROFI_ACTION_URGENT
declare _ROFI_MESSAGE=""
declare _ROFI_SELECTED=""

action() {
    _ROFI_ACTION_NAME+=("$1")
    if [[ ",$3," = *,selected,* ]]; then
        _ROFI_SELECTED=${#_ROFI_ACTION_INFO[@]}
    fi
    if [[ "$2" = "enter-mode "* ]]; then
        _ROFI_ACTION_INFO+=("$2")
    else
        _ROFI_ACTION_INFO+=("$MODE $2")
    fi
}

set_message() {
    _ROFI_MESSAGE="$1"
}

write_rofi_output() {
    if [[ -n "$_ROFI_MESSAGE" ]]; then
        echo -e "\0message\x1f$_ROFI_MESSAGE"
    fi
    if [[ -n "$_ROFI_SELECTED" ]]; then
        echo -e "\0selected\x1f$_ROFI_SELECTED"
    fi
    for ((i=0; i<${#_ROFI_ACTION_NAME[@]};i++)); do
        echo -e "${_ROFI_ACTION_NAME[$i]}\0info\x1f${_ROFI_ACTION_INFO[$i]}"
    done
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
            action "$NAME" "${DEVICE_ID[$NAME]}" "$ATTR"
        else
            action "$NAME disabled" "${DEVICE_ID[$NAME]}" "$ATTR"
        fi
    done
}

process_action() {
    local MODE="$1"
    local ACTION="$2"
    case "$MODE" in
        "")
            case "$ACTION" in
                "")
                    action "Toggle xinput devices" "enter-mode toggle-xinput"
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

process_action "$MODE" "$ACTION"
write_rofi_output

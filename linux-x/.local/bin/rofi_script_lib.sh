if [[ $ROFI_INFO = "enter-mode "* ]]; then
       MODE="${ROFI_INFO#enter-mode }"
       ACTION=""
else
    MODE="${ROFI_INFO%% *}"
    ACTION="${ROFI_INFO#${MODE} }"
fi

if (( ROFI_SCRIPT_DEBUG )); then
    echo "INFO: $ROFI_INFO" >&2
    echo "MODE[$MODE] ACTION[$ACTION]" >&2
fi

declare -a _ROFI_ACTION_NAME
declare -a _ROFI_ACTION_INFO
declare -a _ROFI_ACTION_ACTIVE
declare -a _ROFI_ACTION_URGENT
declare _ROFI_MESSAGE=""
declare _ROFI_SELECTED=""

rofi_action() {
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

rofi_set_message() {
    _ROFI_MESSAGE="$1"
}

_rofi_write_output() {
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

: ${ROFI_SCRIPT_ENTRY:=rofi_script_entry}

trap '"${ROFI_SCRIPT_ENTRY}" "$MODE" "$ACTION" && _rofi_write_output' EXIT

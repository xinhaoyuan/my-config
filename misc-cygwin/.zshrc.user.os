export LC_CTYPE=zh_CN.UTF-8

# reset application working dir to null
local cwd_indicator=$'%{\e]20;\e\\%}'
PROMPT="$PROMPT\${cwd_indicator}"

alias open=cygstart
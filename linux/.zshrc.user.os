LANG="en_US.UTF-8"
LC_CTYPE="zh_CN.UTF-8"

alias open='xdg-open'

# reset application working dir to null
local cwd_indicator=$'%{\e]20;\e\\%}'
PROMPT="$PROMPT\${cwd_indicator}"

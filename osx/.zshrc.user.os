# cwd support for dvtm-mk
local cwd_indicator=$'%{\e]20;%d\e\\%}'
PROMPT="$PROMPT\${cwd_indicator}"

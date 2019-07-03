local theme = dofile("/usr/share/awesome/themes/default/theme.lua")
local dpi   = require("beautiful.xresources").apply_dpi

theme.font = "Sans 9"
theme.mono_font = "Input 9"
theme.useless_gap = dpi(4)

return theme

local theme = dofile("/usr/share/awesome/themes/xresources/theme.lua")
local dpi   = require("beautiful.xresources").apply_dpi

theme.font = "Sans 10"
theme.mono_font = "Hack 10"
theme.useless_gap = dpi(8)
theme.bar_height = dpi(22)
theme.menu_width = dpi(150)
theme.border_width = 0

return theme

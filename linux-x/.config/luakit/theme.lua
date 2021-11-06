--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "10pt Iosevka XY Sans UI"
theme.fg   = "#1c1c1c"
theme.bg   = "#e0e0e0"

-- Genaral colours
theme.success_fg = "#0a0"
theme.loaded_fg  = "#33AADD"
theme.error_fg = "#FFF"
theme.error_bg = "#a00"

-- Warning colours
theme.warning_fg = "#F00"
theme.warning_bg = "#FFF"

-- Notification colours
theme.notif_fg = "#444"
theme.notif_bg = "#FFF"

-- Menu colours
theme.menu_fg                   = "#000"
theme.menu_bg                   = "#fff"
theme.menu_selected_fg          = "#000"
theme.menu_selected_bg          = "#FF0"
theme.menu_title_bg             = "#fff"
theme.menu_primary_title_fg     = "#f00"
theme.menu_secondary_title_fg   = "#666"

theme.menu_disabled_fg = "#999"
theme.menu_disabled_bg = theme.menu_bg
theme.menu_enabled_fg = theme.menu_fg
theme.menu_enabled_bg = theme.menu_bg
theme.menu_active_fg = "#060"
theme.menu_active_bg = theme.menu_bg

-- Proxy manager
theme.proxy_active_menu_fg      = '#000'
theme.proxy_active_menu_bg      = '#FFF'
theme.proxy_inactive_menu_fg    = '#888'
theme.proxy_inactive_menu_bg    = '#FFF'

-- Statusbar specific
theme.sbar_fg         = "#1c1c1c"
theme.sbar_bg         = "#e0e0e0"

-- Downloadbar specific
theme.dbar_fg         = "#fff"
theme.dbar_bg         = "#000"
theme.dbar_error_fg   = "#F00"

-- Input bar specific
theme.ibar_fg           = "#000"
theme.ibar_bg           = "rgba(0,0,0,0)"

-- Tab label
theme.tab_fg            = "#1c1c1c"
theme.tab_bg            = "#e0e0e0"
theme.tab_hover_bg      = "#292929"
theme.tab_ntheme        = "#ddd"
theme.selected_fg       = "#1c1c1c"
theme.selected_bg       = "#aaa"
theme.selected_ntheme   = "#ddd"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#000"

theme.selected_private_tab_bg = "#3d295b"
theme.private_tab_bg    = "#22254a"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#0a0"
theme.notrust_fg        = "#a00"

-- Follow mode hints
theme.hint_font = "8pt Iosevka XY Sans UI, courier, sans-serif"
theme.hint_fg = "#fff"
theme.hint_bg = "#000088"
theme.hint_border = "1px dashed #000"
theme.hint_opacity = "0.3"
theme.hint_overlay_bg = "rgba(255,255,153,0.3)"
theme.hint_overlay_border = "1px dotted #000"
theme.hint_overlay_selected_bg = "rgba(0,255,0,0.3)"
theme.hint_overlay_selected_border = theme.hint_overlay_border

-- General colour pairings
theme.ok = { fg = "#000", bg = "#FFF" }
theme.warn = { fg = "#F00", bg = "#FFF" }
theme.error = { fg = "#FFF", bg = "#F00" }

return theme

-- vim: et:sw=4:ts=8:sts=4:tw=80

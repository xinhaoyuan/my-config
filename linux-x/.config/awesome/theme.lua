local theme = dofile("/usr/share/awesome/themes/xresources/theme.lua")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi   = xresources.apply_dpi
local xrdb  = xresources.get_current_theme()
local gears = require("gears")
local lgi   = require("lgi")
local cairo = lgi.cairo

theme.font = "Sans 10"
theme.mono_font = "Hack 10"
theme.useless_gap = dpi(8)
theme.bar_height = dpi(22)
theme.menu_width = dpi(150)
theme.border_width = 0
theme.titlebar_size = dpi(20)

local function text_to_surface(text, font_desc, color_desc, width, height)
   local surf = lgi.cairo.ImageSurface.create(cairo.Format.ARGB32, width, height)
   local cr = cairo.Context(surf)

   local pl = lgi.Pango.Layout.create(cr)
   pl:set_font_description(font_desc)

   pl:set_text(text)
   local w, h
   w, h = pl:get_size()
   w = w / lgi.Pango.SCALE
   h = h / lgi.Pango.SCALE

   cr:move_to((width - w) / 2, (height - h) / 2)
   cr:set_source(color_desc)
   cr:show_layout(pl)

   return surf
end

local font_desc = beautiful.get_merged_font(theme.mono_font, dpi(10))
local color_desc_normal = gears.color(theme.fg_normal)
local color_desc_focus = gears.color(theme.fg_focus)
local color_desc_hover = gears.color(xrdb.color1)

local function set_titlebar_onetime_button(name, inactive_text, active_text)

   theme["titlebar_" .. name .. "_button_normal"] = text_to_surface(inactive_text, font_desc, color_desc_normal, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_hover"] = text_to_surface(active_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_press"] = text_to_surface(active_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus"] = text_to_surface(inactive_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_hover"] = text_to_surface(active_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_press"] = text_to_surface(active_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)

end

local function set_titlebar_toggle_button(name, inactive_text, active_text)

   theme["titlebar_" .. name .. "_button_normal_inactive"] = text_to_surface(inactive_text, font_desc, color_desc_normal, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_inactive_hover"] = text_to_surface(active_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_inactive_press"] = text_to_surface(active_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_inactive"] = text_to_surface(inactive_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_inactive_hover"] = text_to_surface(active_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_inactive_press"] = text_to_surface(active_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_active"] = text_to_surface(active_text, font_desc, color_desc_normal, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_active_hover"] = text_to_surface(inactive_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_normal_active_press"] = text_to_surface(inactive_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_active"] = text_to_surface(active_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_active_hover"] = text_to_surface(inactive_text, font_desc, color_desc_hover, theme.titlebar_size, theme.titlebar_size)
   theme["titlebar_" .. name .. "_button_focus_active_press"] = text_to_surface(inactive_text, font_desc, color_desc_focus, theme.titlebar_size, theme.titlebar_size)

end

set_titlebar_toggle_button("maximized", "m", "M")
set_titlebar_toggle_button("floating", "f", "F")
set_titlebar_toggle_button("sticky", "s", "S")
set_titlebar_toggle_button("ontop", "t", "T")
set_titlebar_onetime_button("close", "x", "X")

return theme

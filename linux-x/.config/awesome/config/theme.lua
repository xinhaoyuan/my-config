local capi = { mouse = mouse, client = client }
local theme = dofile("/usr/share/awesome/themes/xresources/theme.lua")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi   = xresources.apply_dpi
local xrdb  = xresources.get_current_theme()
local gears = require("gears")
local gcolor = require("gears.color")
local gshape = require("gears.shape")
local fallback = require("fallback")
local masked_imagebox = require("masked_imagebox")
local fixed_place = require("fixed_place")
local aux = require("aux")
local cbg = require("contextual_background")
local lgi   = require("lgi")
local icons = require("icons")
local cairo = lgi.cairo

theme.notification_icon_size = dpi(48)

-- custom property string
-- Interesting fonts: Hack, Iosevka SS08, Lato, Quicksand, Lobster Two, Purisa, Dosis
theme.fontname_normal = "Iosevka SS08"
-- custom property string
theme.fontname_mono = "Iosevka SS08"
theme.font = theme.fontname_normal .. " 10"
-- custom property string font descriptor
theme.font_mono = theme.fontname_mono .. " 10"
theme.useless_gap = dpi(8)
-- custom property number
theme.bar_height = dpi(20)
theme.menu_width = dpi(150)
theme.border_width = dpi(2)
-- custom property color
theme.special_normal = xrdb.color1
-- custom property color
theme.special_focus = xrdb.color11
-- custom property boolean
theme.waffle_use_entire_screen = true
-- custom property color
theme.waffle_background = "#00000000"
-- custom property
theme.waffle_width = dpi(200)

theme.titlebar_bg_focus = theme.bg_normal
theme.titlebar_fg_focus = theme.fg_normal
-- custom property
theme.titlebar_size = theme.bar_height

local function shape_to_surface(shape, fill_color, stroke_color, outer_size, inner_size)
    shape = shape or
        function (cr, size)
            gshape.rectangle(cr, size, size, size / 4)
        end
    local surf = lgi.cairo.ImageSurface.create(cairo.Format.ARGB32, outer_size, outer_size)
    local cr = cairo.Context(surf)
    cr:translate((outer_size - inner_size) / 2, (outer_size - inner_size) / 2)
    shape(cr, inner_size)
    cr:set_source(gcolor(fill_color))
    cr:fill()
    shape(cr, inner_size)
    cr:set_source(gcolor(stroke_color))
    cr:stroke()
    return surf
end

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

local font_desc = beautiful.get_merged_font(theme.font, dpi(10))
local color_desc_normal = gcolor(theme.fg_normal)
local color_desc_focus = gcolor(theme.fg_focus)
local color_desc_hover = gcolor(xrdb.color1)

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

local function set_titlebar_onetime_button_shape(name, shape, ratio, color)
    theme["titlebar_" .. name .. "_button_normal"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * ratio)
    theme["titlebar_" .. name .. "_button_normal_hover"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * ratio)
    theme["titlebar_" .. name .. "_button_normal_press"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * ratio)
    theme["titlebar_" .. name .. "_button_focus"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * ratio)
    theme["titlebar_" .. name .. "_button_focus_hover"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * ratio)
    theme["titlebar_" .. name .. "_button_focus_press"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * ratio)
end

local function set_titlebar_toggle_button_shape(name, shape, inactive_ratio, active_ratio, color)
    theme["titlebar_" .. name .. "_button_normal_inactive"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_normal_inactive_hover"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_normal_inactive_press"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_focus_inactive"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_focus_inactive_hover"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_focus_inactive_press"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * inactive_ratio)
    theme["titlebar_" .. name .. "_button_normal_active"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * active_ratio)
    theme["titlebar_" .. name .. "_button_normal_active_hover"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * active_ratio)
    theme["titlebar_" .. name .. "_button_normal_active_press"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * active_ratio)
    theme["titlebar_" .. name .. "_button_focus_active"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * active_ratio)
    theme["titlebar_" .. name .. "_button_focus_active_hover"] = shape_to_surface(shape, color, theme.border_normal, theme.titlebar_size, theme.titlebar_size * active_ratio)
    theme["titlebar_" .. name .. "_button_focus_active_press"] = shape_to_surface(shape, color, theme.border_focus, theme.titlebar_size, theme.titlebar_size * active_ratio)
end

-- set_titlebar_toggle_button("floating", "f", "F")
-- set_titlebar_toggle_button("maximized", "m", "M")
-- set_titlebar_toggle_button("sticky", "s", "S")
-- set_titlebar_toggle_button("ontop", "t", "T")
-- set_titlebar_onetime_button("close", "x", "X")

local function shape(cr, size)
    gshape.circle(cr, size, size)
    -- gshape.losange(cr, size, size, size)
end
set_titlebar_toggle_button_shape("floating", shape, 0.4, 0.7, xrdb.color13)
set_titlebar_toggle_button_shape("maximized", shape, 0.4, 0.7, xrdb.color10)
set_titlebar_toggle_button_shape("sticky", shape, 0.4, 0.7, xrdb.color11)
set_titlebar_toggle_button_shape("ontop", shape, 0.4, 0.7, xrdb.color12)
set_titlebar_onetime_button_shape("close", shape, 0.6, xrdb.color9)

-- theme.tasklist_shape = function(cr, w, h)
--    offset = h / 4
--    cr:move_to(0, 0)
--    cr:line_to(w, 0)
--    cr:line_to(w - offset, h)
--    cr:line_to(offset, h)
--    cr:close_path()
-- end
-- theme.tasklist_shape_focus = gshape.powerline

-- custom property {"minimal, "simple", "split"}
theme.bar_style = "split"
-- custom property, subset of available styles
theme.bar_styles = {"simple", "split"}
theme.tasklist_plain_task_name = true

local flexer = require("flexer")
-- custom property
theme.tasklist_layout = {
    minimal = {
        forced_height = theme.bar_height,
        layout = wibox.layout.flex.horizontal
    },
    simple = {
        forced_height = theme.bar_height,
        size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
        max_widget_size = dpi(600),
        fill_space = true,
        layout = flexer.horizontal
    },
    split = {
        forced_height = theme.bar_height,
        size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
        layout = flexer.horizontal
    },
}

return theme

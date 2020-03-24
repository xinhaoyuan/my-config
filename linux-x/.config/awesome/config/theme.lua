local capi = { mouse = mouse, client = client }
local theme_assets = require("beautiful.theme_assets")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()
local dpi   = xresources.apply_dpi
local xrdb  = xresources.get_current_theme()
local gears = require("gears")
local gcolor = require("gears.color")
local gshape = require("gears.shape")
local fallback = require("fallback")
local masked_imagebox = require("masked_imagebox")
local fixed_place = require("fixed_place")
local acolor = require("aux").color
local cbg = require("contextual_background")
local lgi   = require("lgi")
local icons = require("icons")
local cairo = lgi.cairo

local c_normal = { xrdb.foreground, xrdb.background }
local c_black  = { xrdb.color0, xrdb.color8 }
local c_red    = { xrdb.color1, xrdb.color9 }
local c_green  = { xrdb.color2, xrdb.color10 }
local c_yellow = { xrdb.color3, xrdb.color11 }
local c_blue   = { xrdb.color4, xrdb.color12 }
local c_purple = { xrdb.color5, xrdb.color13 }
local c_cyan   = { xrdb.color6, xrdb.color14 }
local c_white  = { xrdb.color7, xrdb.color15 }

-- Copied from theme/xresource

local theme = dofile(themes_path.."default/theme.lua")

local function is_light_color(color)
    return acolor(color):lightness() > 0.7
end

theme.bg_normal     = c_normal[2]
theme.bg_focus      = c_blue[2]
theme.bg_urgent     = c_red[2]
theme.bg_minimize   = c_black[2]

theme.bg_systray    = theme.bg_normal

theme.fg_normal     = xrdb.foreground
theme.fg_focus      = is_light_color(theme.bg_focus) and c_black[1] or c_white[2]
theme.fg_urgent     = is_light_color(theme.bg_urgent) and c_black[1] or c_white[2]
theme.fg_minimize   = is_light_color(theme.bg_minimize) and c_black[1] or c_white[2]

theme.useless_gap   = dpi(3)

theme.border_width  = dpi(4)
theme.border_outer_space = dpi(1)
theme.border_inner_space = dpi(1)
theme.border_radius = dpi(12)
theme.border_space = theme.bg_normal
theme.border_focus  = acolor(c_black[2]):blend_with(acolor(theme.bg_focus), 0.5):to_string()
theme.border_normal = acolor(theme.bg_normal):blend_with(acolor(theme.border_focus), 0.3):to_string()
-- theme.border_normal = theme.bg_normal
theme.border_marked = theme.bg_urgent

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

theme.tooltip_fg = theme.fg_normal
theme.tooltip_bg = theme.bg_normal

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(16)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Recolor Layout icons:
theme = theme_assets.recolor_layout(theme, theme.fg_normal)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

theme.notification_icon_size = dpi(48)

-- custom property string
-- Interesting fonts: Hack, Iosevka SS08, Lato, Quicksand, Lobster Two, Purisa, Dosis
theme.fontname_normal = "Iosevka SS08"
-- custom property string
theme.fontname_mono = "Iosevka SS08"
-- custom property number
theme.fontsize_normal = 11
theme.fontsize_small = 9
theme.font = theme.fontname_normal.." "..tostring(theme.fontsize_normal)
-- custom property string font descriptor
theme.font_mono = theme.fontname_mono.." "..tostring(theme.fontsize_normal)
theme.useless_gap = dpi(8)
-- custom property number
theme.bar_height = dpi(22)
theme.menu_width = dpi(150)
-- custom property color
theme.special_normal = is_light_color(theme.bg_normal) and c_red[1] or c_yellow[2]
-- custom property color
theme.special_focus = is_light_color(theme.bg_focus) and c_red[1] or c_yellow[2]
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
    -- gshape.rectangle(cr, size, size)
    gshape.circle(cr, size, size)
    -- gshape.losange(cr, size, size)
end
set_titlebar_toggle_button_shape("floating", shape, 0.4, 0.6, xrdb.color5)
set_titlebar_toggle_button_shape("maximized", shape, 0.4, 0.6, xrdb.color2)
set_titlebar_toggle_button_shape("sticky", shape, 0.4, 0.6, xrdb.color3)
set_titlebar_toggle_button_shape("ontop", shape, 0.4, 0.6, xrdb.color6)
set_titlebar_onetime_button_shape("close", shape, 0.5, xrdb.color1)

-- theme.tasklist_shape = function(cr, w, h)
--    offset = h / 4
--    cr:move_to(0, 0)
--    cr:line_to(w, 0)
--    cr:line_to(w - offset, h)
--    cr:line_to(offset, h)
--    cr:close_path()
-- end
-- theme.tasklist_shape_focus = gshape.powerline

-- custom property {"minimal", "simple", "split", "auto"}
theme.bar_style = "auto"
-- custom property, subset of available styles
theme.bar_styles = {"simple", "split", "auto"}
theme.tasklist_plain_task_name = true

local flexer = require("flexer")
-- custom property
theme.tasklist_layout = {
    horizontal = {
        minimal = {
            forced_height = theme.bar_height,
            layout = wibox.layout.flex.horizontal
        },
        simple = {
            forced_height = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            fill_space = true,
            expand_space = true,
            layout = flexer.horizontal
        },
        split = {
            forced_height = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            layout = flexer.horizontal
        },
        auto = {
            forced_height = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            fill_space = true,
            layout = flexer.horizontal
        },
    },
    vertical = {
        minimal = {
            forced_width = theme.bar_height,
            layout = wibox.layout.flex.vertical
        },
        simple = {
            forced_width = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            fill_space = true,
            expand_space = true,
            layout = flexer.vertical
        },
        split = {
            forced_width = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            layout = flexer.vertical
        },
        auto = {
            forced_width = theme.bar_height,
            size_transform = function (size) return math.min(math.max(dpi(200), size), dpi(600)) end,
            fill_space = true,
            layout = flexer.vertical
        },
    }
}

return theme

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
local border = require("border-theme")
local fallback = require("fallback")
local masked_imagebox = require("masked_imagebox")
local fixed_margin = require("fixed_margin")
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

local function fix_color(color_pair)
    if acolor(color_pair[1]):lightness() > acolor(color_pair[2]):lightness() then
        local tmp = color_pair[1]; color_pair[1] = color_pair[2]; color_pair[2] = tmp
    end
end

if acolor(c_white[1]):lightness() < acolor(c_black[1]):lightness() then
    local tmp = c_white; c_white = c_black; c_black = tmp
end
fix_color(c_white)
fix_color(c_black)

theme.bg_normal     = c_normal[2]
theme.bg_focus      = acolor(c_blue[1]):blend_with(acolor(theme.bg_normal), 0.3):to_string{no_alpha = true}
theme.bg_urgent     = c_red[2]
theme.bg_minimize   = theme.bg_normal

theme.bg_systray    = theme.bg_normal

theme.fg_normal     = c_normal[1]
theme.fg_focus      = is_light_color(theme.bg_focus) and c_black[1] or c_white[2]
theme.fg_urgent     = is_light_color(theme.bg_urgent) and c_black[1] or c_white[2]
theme.fg_minimize   = acolor(is_light_color(theme.bg_minimize) and c_black[1] or c_white[2]):blend_with(acolor(theme.bg_minimize), 0.6):to_string()

-- custom property
theme.sep_normal = acolor(theme.bg_normal):blend_with(acolor(theme.fg_normal), 0.5):to_string()
theme.sep_small_size = dpi(4)
theme.sep_median_size = dpi(6)
theme.sep_big_size = dpi(12)

theme.useless_gap   = dpi(6)

-- Disabling the native border.
theme.border_width = 0

theme.xborder_width  = 9
theme.xborder_outer_space = 3
theme.xborder_inner_space = 3
theme.xborder_radius = dpi(16)
theme.xborder_radius_cut = true
if theme.xborder_radius and theme.xborder_radius > 0 then
    theme.xborder_indent = theme.xborder_radius_cut and math.floor((2 - math.sqrt(2)) * (theme.xborder_radius - theme.xborder_outer_space or 0)) or (theme.xborder_radius - theme.xborder_outer_space or 0)
else
    theme.xborder_indent = dpi(4)
end
theme.xborder_space = is_light_color(theme.bg_normal) and c_white[2] or c_black[1]
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
-- Interesting fonts: Hack, Iosevka SS08 (or SS03), Lato, Quicksand, Lobster Two, Purisa, Dosis
theme.fontname_normal = "Iosevka XY Sans"
-- custom property string
theme.fontname_mono = "Iosevka XY Sans"
-- custom property number
theme.fontsize_normal = 10
theme.fontsize_small = 9
theme.font = theme.fontname_normal.." "..tostring(theme.fontsize_normal)
-- custom property string font descriptor
theme.font_mono = theme.fontname_mono.." "..tostring(theme.fontsize_normal)
theme.useless_gap = dpi(8)
-- custom property number
theme.bar_height = dpi(24)
theme.menu_width = dpi(150)
theme.bar_icon_size = dpi(20)
-- custom property color
theme.special_normal = is_light_color(theme.bg_normal) and c_red[1] or c_yellow[2]
-- custom property color
theme.special_focus = is_light_color(theme.bg_focus) and c_red[1] or c_yellow[2]
-- custom property boolean
theme.waffle_use_entire_screen = true
-- custom property color
theme.waffle_background = "#00000000"
-- custom property number
theme.waffle_panel_width = dpi(200)
-- custom_property number
theme.waffle_item_height = dpi(20)
theme.orgenda_indent_width = dpi(25)

theme.titlebar_bg_focus = theme.bg_normal
theme.titlebar_fg_focus = theme.fg_normal
-- custom property
theme.titlebar_size = dpi(20)
theme.mini_titlebar_size = theme.bar_height
theme.mini_titlebar_width = theme.bar_height
theme.client_default_icon = gcolor.recolor_image(icons.hexagon, theme.fg_normal)

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

local sep_color = gcolor(acolor(theme.fg_normal):blend_with(acolor(theme.bg_normal), 0.75):to_string())
function theme.draw_separator(cr, width, height)
    local dot_r = dpi(1)
    cr:set_source(sep_color)
    cr:set_line_cap("ROUND")
    if width < height then
        -- cr:move_to(width / 2, height / 4)
        -- cr:line_to(width / 2, 3 * height / 4)
        -- cr:stroke()
        -- cr:arc(width / 2, height / 2, dot_r, 0, 2 * math.pi)
        -- cr:fill()
        cr:move_to(width / 2, width / 2)
        cr:line_to(width / 2, height - width / 2)
        cr:set_dash({width <= dpi(12) and dpi(2) or 0, width / 2})
        cr:set_line_width(width / 6)
    else
        cr:move_to(height / 2, height / 2)
        cr:line_to(width - height / 2, height / 2)
        cr:set_dash({width <= dpi(12) and dpi(2) or 0, height / 2})
        cr:set_line_width(height / 6)
    end
    cr:stroke()
end
theme.sep_widget = wibox.widget {
    background = theme.bg_normal,
    bgimage = function(context, cr, width, height)
        theme.draw_separator(cr, width, height)
    end,
    widget = wibox.container.background
}

local function size_transform_function(size) return dpi(400) end
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
            spacing = theme.sep_median_size,
            spacing_widget = theme.sep_widget,
            -- size_transform = size_transform_function,
            fill_space = true,
            -- expand_space = true,
            layout = flexer.horizontal
        },
        split = {
            forced_height = theme.bar_height,
            spacing = theme.sep_median_size,
            spacing_widget = theme.sep_widget,
            -- size_transform = size_transform_function,
            fill_space = true,
            layout = flexer.horizontal
        },
        auto = {
            forced_height = theme.bar_height,
            spacing = theme.sep_median_size,
            spacing_widget = theme.sep_widget,
            -- size_transform = size_transform_function,
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
            -- size_transform = size_transform_function,
            fill_space = true,
            expand_space = true,
            layout = flexer.vertical
        },
        split = {
            forced_width = theme.bar_height,
            -- size_transform = size_transform_function,
            layout = flexer.vertical
        },
        auto = {
            forced_width = theme.bar_height,
            -- size_transform = size_transform_function,
            fill_space = true,
            layout = flexer.vertical
        },
    }
}

-- local function rounded_rect_with_corners(cr, width, height, radius, corners)
--     radius = math.min(radius, width / 2, height / 2)
--     cr:move_to(radius, 0)
--     if corners.top_right then
--         cr:arc( width-radius, radius       , radius, 3*(math.pi/2),    math.pi*2  )
--     else
--         cr:line_to(width, 0)
--         cr:line_to(width, radius)
--     end
--     if corners.bottom_right then
--         cr:arc( width-radius, height-radius, radius,    math.pi*2 ,    math.pi/2  )
--     else
--         cr:line_to(width, height)
--         cr:line_to(width - radius, height)
--     end
--     if corners.bottom_left then
--         cr:arc( radius      , height-radius, radius,    math.pi/2 ,    math.pi    )
--     else
--         cr:line_to(0, height)
--         cr:line_to(0, height - radius)
--     end
--     if corners.top_left then
--         cr:arc( radius      , radius       , radius,    math.pi   , 3*(math.pi/2) )
--     else
--         cr:line_to(0, 0)
--         cr:line_to(radius, 0)
--     end
--     cr:close_path()
-- end

local function get_number(num_or_bool, alt_num)
    return type(num_or_bool) == "number" and num_or_bool
        or (num_or_bool and alt_num or 0)
end

local fac = 2 - math.sqrt(2)
function gshape.partially_cut_rect(cr, width, height, top_left, top_right, bottom_right, bottom_left, radius)
    -- TODO: remove this eveutally
    top_left = get_number(top_left, radius)
    top_right = get_number(top_right, radius)
    bottom_right = get_number(bottom_right, radius)
    bottom_left = get_number(bottom_left, radius)

    cr:new_sub_path()
    cr:move_to(fac * top_left, 0)
    if top_right > 0 then
        cr:line_to(width - fac * top_right , 0)
        cr:line_to(width, fac * top_right)
    else
        cr:line_to(width, 0)
        cr:line_to(width, fac * top_right)
    end

    if bottom_right > 0 then
        cr:line_to(width, height - fac * bottom_right)
        cr:line_to(width - fac * bottom_right, height)
    else
        cr:line_to(width, height)
        cr:line_to(width - fac * bottom_right, height)
    end

    if bottom_left > 0 then
        cr:line_to(fac * bottom_left, height)
        cr:line_to(0, height - fac * bottom_left)
    else
        cr:line_to(0, height)
        cr:line_to(0, height - fac * bottom_left)
    end

    if top_left > 0 then
        cr:line_to(0, fac * top_left)
        cr:line_to(fac * top_left, 0)
    else
        cr:line_to(0, 0)
        cr:line_to(fac * top_left, 0)
    end

    cr:close_path()
end

function gshape.partially_rounded_rect(cr, width, height, top_left, top_right, bottom_right, bottom_left, radius)
    -- TODO: remove this eveutally
    top_left = get_number(top_left, radius)
    top_right = get_number(top_right, radius)
    bottom_right = get_number(bottom_right, radius)
    bottom_left = get_number(bottom_left, radius)

    cr:new_sub_path()

    if top_left > 0 then
        cr:arc(top_left, top_left, top_left, math.pi, 3*(math.pi/2))
    else
        cr:move_to(0,0)
    end

    if top_right > 0 then
        cr:arc(width - top_right, top_right, top_right, 3*(math.pi/2), math.pi*2)
    else
        cr:line_to(width, 0)
    end

    if bottom_right then
        cr:arc(width - bottom_right, height - bottom_right, bottom_right, math.pi*2 , math.pi/2)
    else
        cr:line_to(width, height)
    end

    if bottom_left then
        cr:arc(bottom_left, height - bottom_left, bottom_left, math.pi/2, math.pi)
    else
        cr:line_to(0, height)
    end

    cr:close_path()
end

local border_theme
function theme.get_border_theme()
    if border_theme == nil then
        if beautiful.xborder_radius and beautiful.xborder_radius > beautiful.xborder_width then
            if beautiful.xborder_radius_cut then
                border_theme = setmetatable({}, {__index = border.cut_theme})
            else
                border_theme = setmetatable({}, {__index = border.rounded_theme})
            end
            border_theme.size = beautiful.xborder_radius
            border_theme.outer_space = beautiful.xborder_outer_space
            border_theme.inner_space = beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space
            border_theme:init()
        else
            border_theme = border.default_theme
        end
    end
    return border_theme
end

theme.rect_with_corners = theme.xborder_radius_cut and gshape.partially_cut_rect or gshape.partially_rounded_rect
theme.apply_border_to_widget = function(args)
    args = args or {}
    local widget
    local inner_widget = wibox.widget {
        {
            args.widget,
            widget = fixed_margin,
        },
        shape = beautiful.xborder_radius ~= nil and
            function (cr, width, height)
                beautiful.rect_with_corners(
                    cr, width, height,
                    args.tl ~= false and widget.widget.top > 0 and widget.widget.left > 0,
                    args.tr ~= false and widget.widget.top > 0 and widget.widget.right > 0,
                    args.br ~= false and widget.widget.bottom > 0 and widget.widget.right > 0,
                    args.bl ~= false and widget.widget.bottom > 0 and widget.widget.left > 0,
                    beautiful.xborder_radius -
                    beautiful.xborder_width)
            end,
        bg = beautiful.bg_normal,
        widget = wibox.container.background
    }
    widget = wibox.widget {
        {
            inner_widget,
            top = args.top and beautiful.xborder_width or 0,
            left = args.left and beautiful.xborder_width or 0,
            right = args.right and beautiful.xborder_width or 0,
            bottom = args.bottom and beautiful.xborder_width or 0,
            draw_empty = args.draw_empty,
            widget = fixed_margin,
        },
        bgimage = function (context, cr, width, height)
            if width == 0 or height == 0 then return end
            local tl = args.tl ~= false and widget.widget.top > 0 and widget.widget.left > 0
            local tr = args.tr ~= false and widget.widget.top > 0 and widget.widget.right > 0
            local br = args.br ~= false and widget.widget.bottom > 0 and widget.widget.right > 0
            local bl = args.bl ~= false and widget.widget.bottom > 0 and widget.widget.left > 0
            local indicator = args.indicator ~= false and beautiful.xborder_radius and beautiful.xborder_radius > beautiful.xborder_width and beautiful.xborder_radius_cut
            if beautiful.xborder_radius then
                cr:set_source(gcolor(beautiful.xborder_space))
                if indicator then
                    gshape.rectangle(cr, width, height)
                else
                    beautiful.rect_with_corners(
                        cr, width, height,
                        tl, tr, br, bl,
                        beautiful.xborder_radius)
                end
                if args.clip then
                    cr:clip()
                else
                    cr:fill()
                    cr:set_operator('ATOP')
                end
            end
            border:draw({ theme = beautiful.get_border_theme(),
                          color = beautiful.border_focus }, cr, width, height,
                border.directions{
                    widget.widget.top > 0 and "top",
                    widget.widget.left > 0 and "left",
                    widget.widget.right > 0 and "right",
                    widget.widget.bottom > 0 and "bottom"
            })
            if indicator then
                cr:set_source(gcolor(beautiful.fg_normal))
                local indent = (2 - math.sqrt(2)) * (beautiful.xborder_radius) - beautiful.xborder_outer_space
                if tl then
                    cr:move_to(beautiful.xborder_outer_space, beautiful.xborder_outer_space)
                    cr:line_to(beautiful.xborder_outer_space, indent)
                    cr:line_to(indent, beautiful.xborder_outer_space)
                    cr:fill()
                end

                if tr then
                    cr:move_to(width - indent, beautiful.xborder_outer_space)
                    cr:line_to(width - beautiful.xborder_outer_space, beautiful.xborder_outer_space)
                    cr:line_to(width - beautiful.xborder_outer_space, indent)
                    cr:fill()
                end

                if br then
                    cr:move_to(width - indent, height - beautiful.xborder_outer_space)
                    cr:line_to(width - beautiful.xborder_outer_space, height - beautiful.xborder_outer_space)
                    cr:line_to(width - beautiful.xborder_outer_space, height - indent)
                    cr:fill()
                end

                if bl then
                    cr:move_to(indent, height - beautiful.xborder_outer_space)
                    cr:line_to(beautiful.xborder_outer_space, height - beautiful.xborder_outer_space)
                    cr:line_to(beautiful.xborder_outer_space, height - indent)
                    cr:fill()
                end
            end
        end,
        widget = wibox.container.background
    }
    return widget
end


return theme

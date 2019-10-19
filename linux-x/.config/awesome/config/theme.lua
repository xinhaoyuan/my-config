local theme = dofile("/usr/share/awesome/themes/xresources/theme.lua")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi   = xresources.apply_dpi
local xrdb  = xresources.get_current_theme()
local gears = require("gears")
local fallback = require("fallback")
local lgi   = require("lgi")
local icons = require("icons")
local cairo = lgi.cairo

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
local default_icon = gears.color.recolor_image(icons.terminal, theme.fg_normal)

local property_to_text = {
   {"sticky", "S"},
   {"ontop", "T"},
   {"maximized", "M"},
   {"floating", "F"},
}

local function tasklist_update_function(widget, c, index, objects)
    local tb = widget:get_children_by_id("text_role")
    if tb[1] then
        tb[1].is_odd_child = index % 2 == 1
    end
    local sb = widget:get_children_by_id("status_role")
    local status_markup = ""
    local prop = {}
    for _, pp in ipairs(property_to_text) do
        local key = pp[1]
        if c.saved and c.saved[key] ~= nil then
            prop[key] = c.saved[key]
        elseif c[key] ~= nil then
            prop[key] = c[key]
        end
    end
    for _, pp in ipairs(property_to_text) do
        local key, text = table.unpack(pp)
        if prop[key] == true then
            if key ~= "floating" or not prop.maximized then 
                status_markup = status_markup .. text
            end
        end
    end
    if #status_markup > 0 then
        sb[1].markup = "<span color='" .. ((client.focus == c or c.minimized) and theme.special_focus or theme.special_normal) .. "'>" .. status_markup .. "</span>"
    else
        sb[1].markup = ""
    end
end

theme.tasklist_template = {
   {
      {
         {
            {
               {
                  widget = awful.widget.clienticon,
               },
               {
                  image = default_icon,
                  widget = wibox.widget.imagebox,
               },
               widget = fallback,
            },
            right = dpi(3),
            widget = wibox.container.margin,
         },
         {
            id = "text_role",
            widget = wibox.widget.textbox,
         },
         {
            {
               id = "status_role",
               widget = wibox.widget.textbox,
            },
            left = dpi(3),
            widget = wibox.container.margin,
         },
         layout = wibox.layout.align.horizontal,
      },
      -- id = "text_margin_role",
      left  = dpi(5),
      right = dpi(5),
      widget = wibox.container.margin
   },
   id     = "background_role",
   widget = wibox.container.background,
   create_callback = tasklist_update_function,
   update_callback = tasklist_update_function,
}

local flexer = require("flexer")
-- custom property
theme.tasklist_layout = {
    minimal = {
        forced_height = theme.bar_height,
        layout = wibox.layout.flex.horizontal
    },
    simple = {
        forced_height = theme.bar_height,
        size_transform = function (size) return math.min(math.max(math.ceil(size / dpi(60)), 5), 10) * dpi(60) end,
        max_widget_size = dpi(600),
        fill_space = true,
        layout = flexer.horizontal
    },
    split = {
        forced_height = theme.bar_height,
        size_transform = function (size) return math.min(math.max(math.ceil(size / dpi(60)), 5), 10) * dpi(60) end,
        layout = flexer.horizontal
    },
}

-- custom property
theme.titlebar_size = theme.bar_height

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

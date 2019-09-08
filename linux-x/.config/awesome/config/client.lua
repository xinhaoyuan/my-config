local capi = {
   awesome = awesome,
   client = client,
}
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi
local gtimer = require("gears.timer")
local machi = require("layout-machi")

local table_join = awful.util.table.join
local delayed = gtimer.delayed_call

local client_keys = table_join(
   awful.key({ "Mod4" }, "Tab", function (c)
         c.maximized = false
         c.maximized_vertical = false
         c.maximized_horizontal = false
         c.fullscreen = false
         c.floating = false
         c:raise()
         delayed(
            function ()
               machi.switcher.start(c)
            end
         )
   end),

   awful.key({ "Mod4" }, "=", function (c)
         if c.fullscreen then
         elseif c.minimized then
            c.minimized = false
         elseif not c.maximized then
            c.maximized = true
         else
            c.fullscreen = true
         end
   end),

   awful.key({ "Mod4" }, "-", function (c)
         if c.fullscreen then
            c.fullscreen = false
         elseif c.maximized then
            c.maximized = false
         elseif not c.minimized then
            c.minimized = true
         end
   end),

   awful.key({ "Mod4" }, "t", function (c) awful.titlebar.toggle(c) end),

   awful.key({ "Mod4" }, "f", function (c)
         if c.fullscreen then
         else
            if c.floating then
               c.maximized = false;
               c.maximized_vertical = false;
               c.maximized_horizontal = false;
            end
            awful.client.floating.toggle(c);
         end
   end),

   awful.key({ "Mod4" }, "c", function (c) c:kill() end)
)

local client_buttons = table_join(
   awful.button({ }, 1, function (c) capi.client.focus = c; c:raise() end),
   awful.button({ "Mod4" }, 1, function (c) awful.mouse.client.move(c) end),
   awful.button({ "Mod4" }, 3, function (c)
         local _, cc = awful.placement.closest_corner(mouse, {parent = c})
         awful.mouse.client.resize(c, cc)
   end)
)

-- gain focus and raise before moving

awful.mouse.resize.set_mode("live")

awful.mouse.resize.add_enter_callback(
   function (c)
      c:emit_signal("request::activate", "mouse.move", {raise=false})
      c:raise()
   end, 'mouse.move')

awful.mouse.resize.add_enter_callback(
   function (c)
      c:emit_signal("request::activate", "mouse.resize", {raise=false})
      c:raise()
   end, 'mouse.resize')


capi.client.connect_signal(
   "request::titlebars",
   function (c)
      -- buttons for the titlebar
      local buttons = awful.util.table.join(
         awful.button({ }, 1, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.move(c)
         end),
         awful.button({ }, 2, function()
               awful.titlebar.hide(c)
         end),
         awful.button({ }, 3, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.resize(c)
         end)
      )

      local titlewidget = awful.titlebar.widget.titlewidget(c)
      titlewidget:set_font(mono_font)
      awful.titlebar(
         c,
         {
            size = beautiful.titlebar_size,
            font = mono_font,
         }
      ):setup
      {
         { -- Left
            awful.titlebar.widget.iconwidget(c),
            titlewidget,
            spacing = dpi(2),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
         },
         { -- Space
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
         },
         { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
         },
         layout = wibox.layout.align.horizontal,
      }
   end
)

-- change border color based on focus
capi.client.connect_signal(
   "focus",
   function (c)
      c.border_color = beautiful.border_focus
   end
)
capi.client.connect_signal(
   "unfocus",
   function (c)
      c.border_color = beautiful.border_normal
   end
)

-- remove border for maximized windows
function reset_border(c)
   if not c.borderless and not c.maximized then
      c.border_width = beautiful.border_width
   else
      c.border_width = 0
   end
end

capi.client.connect_signal("manage", reset_border)
capi.client.connect_signal("property::maximized", reset_border)

-- rules

require("awful.rules").rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = false,
         keys = client_keys,
         buttons = client_buttons,
         border_color = beautiful.border_normal,
         screen = function(c) return capi.awesome.startup and c.screen or awful.screen.focused() end,
         floating = true,
         placement = awful.placement.centered,
         border_width = 0,
      }
   },
   {
      rule = { class = "Synapse" },
      properties = {
         ontop = true,
      },
   },
   {
      rule = { class = "Plank", type = "dock" },
      properties = {
         floating = true,
         sticky = true,
         ontop = true,
         focusable = false,
         below = false,
         has_client_input_shape = true,
         borderless = true,
      },
   },
   {
      rule = { class = "Conky" },
      properties = {
         floating = true,
         sticky = true,
         ontop = false,
         below = true,
         focus = false,
         borderless = true,
         focusable = false,
      }
   },
}

return nil

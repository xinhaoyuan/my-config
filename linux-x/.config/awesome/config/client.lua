local capi = {
   awesome = awesome,
   client = client,
   mouse = mouse,
}

local shared = require((...):match("(.-)[^%.]+$") .. "shared")
shared.client = {}

local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi
local gtimer = require("gears.timer")
local machi = require("layout-machi")

function shared.client.titlebar_toggle(c)
   local geo = c:geometry()
   awful.titlebar.toggle(c)
   c:geometry(geo)
end

function shared.client.titlebar_show(c)
   local geo = c:geometry()
   awful.titlebar.show(c)
   c:geometry(geo)
end

function shared.client.titlebar_hide(c)
   local geo = c:geometry()
   awful.titlebar.hide(c)
   c:geometry(geo)
end

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

   awful.key({ "Mod4" }, "t", function (c) shared.client.titlebar_toggle(c) end),

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
         local _, cc = awful.placement.closest_corner(capi.mouse, {parent = c})
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
               shared.client.titlebar_toggle(c)
         end),
         awful.button({ }, 3, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.resize(c)
         end)
      )

      local titlewidget = awful.titlebar.widget.titlewidget(c)
      titlewidget:set_font(beautiful.font)
      local titlebar_container = wibox.widget {
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
         },
         bottom = beautiful.border_width,
         color = capi.client.focus == c and beautiful.border_focus or beautiful.border_normal,
         widget = wibox.container.margin,
      }
      awful.titlebar(
         c,
         {
            size = beautiful.titlebar_size + beautiful.border_width,
            font = beautiful.font,
         }
      ):setup({ titlebar_container, widget = wibox.container.margin })
      c.titlebar_container = titlebar_container
   end
)

-- change border color based on focus
capi.client.connect_signal(
   "focus",
   function (c)
      c.border_color = beautiful.border_focus
      if c.titlebar_container then
         c.titlebar_container.color = beautiful.border_focus
      end
   end
)
capi.client.connect_signal(
   "unfocus",
   function (c)
      c.border_color = beautiful.border_normal
      if c.titlebar_container then
         c.titlebar_container.color = beautiful.border_normal
      end
   end
)

-- remove border for maximized windows
local function reset_border(c)
   if not c.borderless and not c.maximized then
      c.border_width = beautiful.border_width
   else
      c.border_width = 0
   end
end

local function manage_cb(c)
   reset_border(c)
   if shared.var.enable_titlebar then
      shared.client.titlebar_show(c)
   end
end

capi.client.connect_signal("manage", manage_cb)
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
         floating = shared.var.floating_by_default,
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

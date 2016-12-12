local aw = require("awful")
local ar = require("awful.rules")
local af = require("awful.autofocus")
local na = require("naughty")
local be = require("beautiful")
-- 3rd party libs
local cyclefocus = require("cyclefocus")

aw.util.spawn_with_shell("$HOME/.xdesktoprc.awesome")
be.init("/usr/share/awesome/themes/default/theme.lua")

-- Layouts
local layouts = {
   aw.layout.suit.tile,
   aw.layout.suit.fair,
   aw.layout.suit.max
}

tags = {}
for s = 1, screen.count() do
   tags[s] = aw.tag({ 1, 2 ,3, 4 }, s, layouts[1])
end

-- reserve room for conky

dummy_status_bar = aw.wibox({ position = "top", screen = 1, ontop = false, width = 1, height = 40 })

-- Keys and buttons
local global_keys = aw.util.table.join(
   aw.key({ "Mod4" }, "Left", aw.tag.viewprev),
   aw.key({ "Mod4" }, "Right", aw.tag.viewnext),
   
   aw.key({ "Mod4" }, "r", function () aw.util.spawn_with_shell("dlauncher open") end),
   aw.key({ "Mod4" }, "Return", function () aw.util.spawn("open-terminal-emulator") end),
   aw.key({ "Mod4" }, "space", function () aw.util.spawn("urxvt -name root-terminal") end),
   
   aw.key({ "Mod4" }, "[", function () aw.layout.inc(layouts, -1) end),
   aw.key({ "Mod4" }, "]", function () aw.layout.inc(layouts, 1) end),
   
   aw.key({ "Mod1" }, "Tab", function () cyclefocus.cycle(1, { modifier = "Alt_L" }) end),

   aw.key({ "Mod4" }, "h", function () aw.tag.incmwfact(-0.1) end),
   aw.key({ "Mod4" }, "l", function () aw.tag.incmwfact( 0.1) end),
   aw.key({ "Mod4" }, "j", function () aw.client.incwfact(-0.1) end),
   aw.key({ "Mod4" }, "k", function () aw.client.incwfact( 0.1) end),

   aw.key({ "Mod4", "Control" }, "q", awesome.quit)
)

local client_keys = aw.util.table.join(
   aw.key({ "Mod4" }, "w", function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c.maximized_vertical = not c.maximized_vertical
   end),
   aw.key({ "Mod4" }, "c", function (c) c:kill() end),
   aw.key({ "Mod4" }, "z", function (c) c:swap(aw.client.getmaster()) end)
)

local client_buttons = aw.util.table.join(
   aw.button({ }, 1, function (c) client.focus = c; c:raise() end),
   aw.button({ "Mod4" }, 1, aw.mouse.client.move),
   aw.button({ "Mod4" }, 3, aw.mouse.client.resize)
)

root.keys(global_keys)

-- rules

client.connect_signal(
   "focus",
   function (c) c.border_color = be.border_focus end)
client.connect_signal(
   "unfocus",
   function (c) c.border_color = be.border_normal end)

ar.rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = false,
         keys = client_keys,
         buttons = client_buttons,
         border_width = 4,
         border_color = be.border_normal
      }
   },
   {
      rule  = { class = "Conky" },
      properties = {
         floating = true,
         sticky = true,
         ontop = false,
         below = true,
         focus = false,
         border_width = 0,
         focusable = false
      }
   }
}


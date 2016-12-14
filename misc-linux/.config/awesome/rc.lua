local aw = require("awful")
local ar = require("awful.rules")
local af = require("awful.autofocus")
local na = require("naughty")
local be = require("beautiful")
-- 3rd party libs
local cf = require("cyclefocus")

na.config.defaults.font = "Sans 32"

function debug(msg)
   na.notify({
         text = tostring(msg),
         timeout = 10,
   })
end

aw.util.spawn_with_shell("$HOME/.xdesktoprc.awesome")
be.init("/usr/share/awesome/themes/default/theme.lua")

-- layouts
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

function is_floating (c)
   return c.floating or c.maximized_vertical or c.maximized_horizontal or c.type == "dialog"
end

-- keys and buttons
local global_keys = aw.util.table.join(
   aw.key({ "Mod4" }, "Left", aw.tag.viewprev),
   aw.key({ "Mod4" }, "Right", aw.tag.viewnext),
   
   aw.key({ "Mod4" }, "r", function () aw.util.spawn_with_shell("dlauncher open") end),
   aw.key({ "Mod4" }, "Return", function () aw.util.spawn("open-terminal-emulator") end),
   aw.key({ "Mod4" }, "space", function () aw.util.spawn("urxvt -name root-terminal") end),
   
   aw.key({ "Mod4" }, "[", function () aw.layout.inc(layouts, -1) end),
   aw.key({ "Mod4" }, "]", function () aw.layout.inc(layouts, 1) end),

   aw.key({ "Mod4" }, "h", function () aw.tag.incmwfact(-0.05) end),
   aw.key({ "Mod4" }, "l", function () aw.tag.incmwfact( 0.05) end),
   aw.key({ "Mod4" }, "j", function () aw.client.incwfact(-0.1) end),
   aw.key({ "Mod4" }, "k", function () aw.client.incwfact( 0.1) end),

   aw.key({ }, "XF86AudioLowerVolume", function() aw.util.spawn("amixer sset Master,0 2%-") end),
   aw.key({ }, "XF86AudioRaiseVolume", function() aw.util.spawn("amixer sset Master,0 2%+") end),
   aw.key({ }, "XF86AudioMute", function() aw.util.spawn("amixer sset Master,0 toggle") end),

   aw.key({ "Mod4", "Control" }, "q", awesome.quit)
)

local client_keys = aw.util.table.join(
   cf.key({ "Mod1" }, "Tab", 1,
      {
         modifier = "Alt_L",
         cycle_filters = {
            cf.filters.same_screen, cf.filters.common_tag
            ,
            function (c, src_c)
               if c.pid == src_c.pid then return true
               else
                  return is_floating(c) == is_floating(src_c)
               end
            end
         }
   }),
   aw.key({ "Mod4" }, "Tab", function(src_c)
         local f = is_floating(src_c)
         local new_focus = nil
         for _, c in ipairs(client.get(src_c.screen)) do
            if c:isvisible() and (not aw.client.focus.filter or aw.client.focus.filter(c)) then
               if not is_floating(c) then
                  if f then
                     c:raise()
                     if not new_focus then
                        new_focus = c
                     end
                  else
                     c:lower()
                  end
               else
                  if not f and not new_focus then
                     new_focus = c
                  end
               end
            end

            if new_focus then
               client.focus = new_focus
            end
         end
   end),

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
      rule = { class = "Conky" },
      properties = {
         floating = true,
         sticky = true,
         ontop = false,
         below = true,
         focus = false,
         border_width = 0,
         focusable = false
      }
   },
}

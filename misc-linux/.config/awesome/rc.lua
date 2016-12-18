local aw = require("awful")
local ar = require("awful.rules")
local na = require("naughty")
local be = require("beautiful")
local wi = require("wibox")
-- 3rd party libs
local cfg = require("my-config")
local ut = require("my-utils")
local af = require("my-autofocus")
local cf = require("cyclefocus")
local ch = require("conky-hud")

local HOME_DIR = os.getenv("HOME")
na.config.defaults.font = "Sans " .. (10 * cfg.font_scale_factor)
cf.naughty_preset.position = "center_middle"

local debug = function (msg)
   na.notify({
         text = tostring(msg),
         timeout = 10
   })
end

awesome.connect_signal(
   "debug::error",
   function (msg)
      debug(msg)
   end
)

aw.util.spawn_with_shell(HOME_DIR .. "/.xdesktoprc.awesome")
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

require("my-widgets")

-- helper functions

local is_floating = function (c)
   return c.floating or c.maximized_vertical or c.maximized_horizontal or c.type == "dialog"
end

af.find_alternative_focus = function(c)
   local f = is_floating(c)
   local new_focus = cf.find_history(
      0, {
         function (nc)
            return
               nc.valid
               and cf.filters.same_screen(nc, c)
               and is_floating(nc) == f
         end
   })

   if new_focus then return new_focus end

   new_focus = cf.find_history(
      0, {
         function (nc)
            return
               nc.valid
               and cf.filters.same_screen(nc, c)
         end
   })

   return new_focus
end

-- keys and buttons
local global_keys = aw.util.table.join(
   aw.key({ "Mod4", "Control" }, "Left", aw.tag.viewprev),
   aw.key({ "Mod4", "Control" }, "Right", aw.tag.viewnext),
      
   aw.key({ "Mod4" }, "[", function () aw.layout.inc(layouts, -1) end),
   aw.key({ "Mod4" }, "]", function () aw.layout.inc(layouts, 1) end),

   aw.key({ }, "XF86AudioLowerVolume", function() aw.util.spawn("amixer sset Master,0 2%-") end),
   aw.key({ }, "XF86AudioRaiseVolume", function() aw.util.spawn("amixer sset Master,0 2%+") end),
   aw.key({ }, "XF86AudioMute", function() aw.util.spawn("amixer sset Master,0 toggle") end),
   aw.key({ }, "XF86MonBrightnessUp", function () aw.util.spawn("xbacklight -inc 5") end),
   aw.key({ }, "XF86MonBrightnessDown", function () aw.util.spawn("xbacklight -dec 5") end),   

   aw.key({ "Mod4", "Control" }, "m", function () for _, c in pairs(client.get()) do c.minimized = false end end),

   aw.key({ "Mod4" }, "r", function () aw.util.spawn_with_shell("dlauncher open") end),
   aw.key({ "Mod4" }, "Return", function () aw.util.spawn(HOME_DIR .. "/bin/open-terminal-emulator") end),
   aw.key({ "Mod4" }, "t", function () aw.util.spawn("urxvt -name root-terminal") end),

   aw.key({ "Mod4" }, "F1", function () ch.toggle_conky() end),
   aw.key({ "Mod4" }, "grave", function() ch.raise_conky() end, function() ch.lower_conky_delayed() end),
   
   aw.key({ "Mod4", "Control" }, "Escape", awesome.quit)
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
                  else
                     c:lower()
                  end
               else
               end
            end            
         end

         new_focus = cf.find_history(
            0, {
               function (c)
                  return cf.filters.same_screen(c, src_c)
                     and cf.filters.common_tag(c, src_c)
                     and is_floating(c) ~= f
               end
         })
         
         if new_focus then
            client.focus = new_focus
         end
   end),

   aw.key({ "Mod4" }, "Up", function (c)
         if c.minimized then
            c.minimized = false
         else
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical = not c.maximized_vertical
         end
   end),
   aw.key({ "Mod4" }, "Down", function (c)
         c.minimized = true
   end),
      
   aw.key({ "Mod4" }, "Left", function (c)
         if not is_floating(c) then
            c:swap(aw.client.getmaster())
         end
   end),

   aw.key({ "Mod4" }, "Right", function (c) aw.client.floating.toggle(c) end),

   aw.key({ "Mod4" }, "w", function (c) aw.client.focus.bydirection("up"); client.focus:raise() end),
   aw.key({ "Mod4" }, "a", function (c) aw.client.focus.bydirection("left"); client.focus:raise() end),
   aw.key({ "Mod4" }, "s", function (c) aw.client.focus.bydirection("down"); client.focus:raise() end),
   aw.key({ "Mod4" }, "d", function (c) aw.client.focus.bydirection("right"); client.focus:raise() end),

   aw.key({ "Mod4" }, "j", function (c) aw.tag.incmwfact(-0.05) end),
   aw.key({ "Mod4" }, "l", function (c) aw.tag.incmwfact( 0.05) end),
   aw.key({ "Mod4" }, "i", function (c) aw.client.incwfact(-0.1) end),
   aw.key({ "Mod4" }, "k", function (c) aw.client.incwfact( 0.1) end),


   aw.key({ "Mod4" }, "c", function (c) c:kill() end)
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
   function (c)
      c.border_color = be.border_focus
   end
)
client.connect_signal(
   "unfocus",
   function (c)
      c.border_color = be.border_normal      
   end
)

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

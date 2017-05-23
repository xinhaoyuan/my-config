-- need to do this before everything, so that signal handler fires before standard ones
client.connect_signal(
   "unmanage",
   function (c)
      c.tomb_floating = c.floating
      c.tomb_class = c.class
      c.tomb_pid = c.pid
   end
)

local aw = require("awful")
local ar = require("awful.rules")
local al = require("awful.layout")
local ak = require("awful.keygrabber")
local na = require("naughty")
local be = require("beautiful")
local wi = require("wibox")
local gtimer = require("gears.timer")
local kg = keygrabber
-- 3rd party libs
local cfg = require("my-config")
local ut = require("my-utils")
local af = require("my-autofocus")
local cf = require("cyclefocus")
-- local sw = require("awesome-switcher-preview")
local ch = require("conky-hud")

local HOME_DIR = os.getenv("HOME")
na.config.defaults.font = "Sans " .. (10 * cfg.font_scale_factor)
cf.naughty_preset.position = "center_middle"

awesome.connect_signal(
   "debug::error",
   function (msg)
      print(msg)
   end
)

local spawn = function(cmd, to_notify)
   aw.spawn.spawn(cmd)
end

local spawn_with_shell = function(cmd, to_notify)
   aw.spawn.with_shell(cmd)
end

aw.spawn.with_shell(HOME_DIR .. "/.xdesktoprc.awesome")
be.init("/usr/share/awesome/themes/default/theme.lua")

-- layouts

local tag_list = { "1", "2", "3", "4", "5", "6", "STICKY" }
local global_keys_switch_tags = {}
local tag_index = 0
for i, t in ipairs(tag_list) do
   if cfg.tag_filter(t) then
      tag_index = tag_index + 1
      if tag_index >= 10 then break end

      global_keys_switch_tags = aw.util.table.join(
         global_keys_switch_tags,
         aw.key({ "Mod4" }, tostring(tag_index), function () aw.screen.focused().tags[i]:view_only() end),
         aw.key({ "Mod4", "Control" }, tostring(tag_index),
            function ()
               local c = client.focus
               if c == nil then return end
               aw.client.toggletag(c.screen.tags[i], c)
            end
         )
      )
   end
end

local layouts = {
   al.suit.tile,
   al.suit.fair,
   al.suit.max
}

tags = {}
for s = 1, screen.count() do
   tags[s] = aw.tag(tag_list, s, layouts[1])
end

require("my-widgets")

-- helper functions

local is_floating = function (c)
   return
      c.tomb_floating or c.floating
      or c.maximized_horizontal or c.maximized_vertical or c.maximized or c.fullscreen
      or #al.parameters(nil, c.screen).clients <= 1
      or c.type == "dialog"
end

af.find_alternative_focus = function(prev, s)
   local f = nil
   local pid = nil
   if prev and prev.valid then
      f = is_floating(prev)
      pid = prev.tomb_pid or prev.pid
   end

   local filters = {}
   if pid then
      filters[#filters + 1] = function (c)
         return c.valid and c:isvisible() and c.pid == pid
      end
   end

   filters[#filters + 1] = function (c)
      return c.valid and c:isvisible() and (f == nil or is_floating(c) == f)
   end

   return cf.find_first_in_history(filters, true)
end

local my_focus_by_direction = function(dir)
   local old_c = client.focus

   if old_c ~= nil and old_c.screen ~= aw.screen.focused() then
      aw.screen.focus(old_c.screen.index)
   end

   aw.client.focus.global_bydirection(dir);
   local new_c = client.focus

   if old_c ~= new_c and new_c ~= nil then
      aw.screen.focus(new_c.screen.index)
      new_c:raise()
   end
end

local win_pressed = function ()
   kg.run(
      function (mod, key, event)
         -- ??? bug
         if key == " " then key = "space" end
         if event == "press" and key ~= "Super_L" then
            local c = client.focus
            if c ~= nil then
               for _, k in ipairs(c:keys()) do
                  if aw.key.match(k, mod, key) then
                     kg.stop()
                     k:emit_signal("press", c)
                     return
                  end
               end
            end

            for i, k in ipairs(root.keys()) do
               if aw.key.match(k, mod, key) then
                  kg.stop()
                  k:emit_signal("press")
                  return
               end
            end
         elseif event == "release" and key == "Super_L" then
            -- win key released without any key triggered
            spawn_with_shell("dlauncher open")
            kg.stop()
         end
   end)
end

-- WIP
local window_edit_mode = {}
function window_edit_mode.start (c)
   kg.run(
      function (mod, key, event)
         if event == "release" then return end
         if key == "Return" or key == "Escape" then kg.stop(); return end

         local shift = false
         local ctrl = false
         local alt = false
         for _, m in ipairs(mod) do
            if m == "Shift" then shift = true
            elseif m == "Control" then ctrl = true
            elseif m == "Mod1" then alt = true
            end
         end
   end)
end

-- keys and buttons
local global_keys = aw.util.table.join(
   global_keys_switch_tags,

   aw.key({ "Mod4" }, "[", function () al.inc(layouts, -1) end),
   aw.key({ "Mod4" }, "]", function () al.inc(layouts, 1) end),

   aw.key({ "Mod1" }, "Tab",
      function ()
         cf.cycle(
            1,
            {
               initiating_client = client.focus,
               keys = { "Tab" },
               modifier = "Alt_L",
               cycle_filters = {
                  -- cf.filters.same_screen, cf.filters.common_tag
                  function (c, src_c)
                     return c:isvisible()
                  end
                  ,
                  function (c, src_c)
                     if src_c == nil then return true end
                     if c.pid == src_c.pid then return true
                     else
                        return is_floating(c) == is_floating(src_c)
                     end
                  end
               }
            })
      end
   ),

   aw.key({ "Mod4" }, "w", function () my_focus_by_direction("up") end),
   aw.key({ "Mod4" }, "a", function () my_focus_by_direction("left") end),
   aw.key({ "Mod4" }, "s", function () my_focus_by_direction("down") end),
   aw.key({ "Mod4" }, "d", function () my_focus_by_direction("right") end),

   -- aw.key({ "Mod1",           }, "Tab",
   --    function ()
   --       sw.switch( 1, "Alt_L", "Tab", "ISO_Left_Tab")
   -- end),
   -- aw.key({ "Mod1", "Shift"   }, "Tab",
   --    function ()
   --       sw.switch(-1, "Alt_L", "Tab", "ISO_Left_Tab")
   -- end),

   aw.key({ }, "XF86AudioLowerVolume", function() spawn("amixer sset Master,0 2%-") end),
   aw.key({ }, "XF86AudioRaiseVolume", function() spawn("amixer sset Master,0 2%+") end),
   aw.key({ }, "XF86AudioMute", function() spawn("amixer sset Master,0 toggle") end),
   aw.key({ }, "XF86MonBrightnessUp", function () spawn("xbacklight -inc 5") end),
   aw.key({ }, "XF86MonBrightnessDown", function () spawn("xbacklight -dec 5") end),

   aw.key({ "Mod4", "Control" }, "m", function () for _, c in pairs(client.get()) do c.minimized = false end end),

   aw.key({ "Mod4" }, "r", function () spawn_with_shell("dlauncher open") end),
   aw.key({ "Mod4" }, "Return", function () spawn_with_shell("open-terminal-emulator " .. aw.screen.focused().index .. "-" .. aw.screen.focused().selected_tag.index) end),
   aw.key({ "Mod4", "Control" }, "Return", function () spawn_with_shell("open-terminal-emulator background") end),
   aw.key({ "Mod4" }, "t", function () spawn("urxvt -name root-terminal") end),

   aw.key({ "Mod4" }, "F1", function () ch.toggle_conky() end),
   aw.key({ "Mod4" }, "F2", function () spawn("pcmanfm") end),
   aw.key({ "Mod4" }, "F3", function () spawn("zim") end),
   aw.key({ "Mod4" }, "grave", function() ch.raise_conky() end, function() ch.lower_conky_delayed() end),
   -- disabled because of conflict with IME
   -- aw.key({ }, "Super_L", win_pressed),

   aw.key({ "Mod4", "Control" }, "Escape", awesome.quit)
)

local client_keys = aw.util.table.join(
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

         new_focus = cf.find_first_in_history(
            {
               function (c)
                  return cf.filters.same_screen(c, src_c)
                     and cf.filters.common_tag(c, src_c)
                     and is_floating(c) ~= f
               end
            }
         )

         if new_focus then
            client.focus = new_focus
         end
         client.focus:raise()
   end),

   aw.key({ "Mod4" }, "Up", function (c)
         if c.minimized then
            c.minimized = false
         else
            c.maximized = not c.maximized
         end
   end),

   aw.key({ "Mod4" }, "f", function (c)
         c.fullscreen = not c.fullscreen
   end),

   -- aw.key({ "Mod4" }, "Down", function (c)
   --       c.minimized = true
   -- end),

   aw.key({ "Mod4" }, "Left", function (c)
         if not is_floating(c) then
            c:swap(aw.client.getmaster())
         end
   end),

   aw.key({ "Mod4" }, "Right", function (c)
         if c.floating then
            c.maximized = false;
            c.maximized_vertical = false;
            c.maximized_horizontal = false;
         end
         aw.client.floating.toggle(c);
   end),

   aw.key({ "Mod4", "Control" }, "w", function (c) aw.client.swap.global_bydirection("up"); gtimer.delayed_call(function () client.focus = c; c:raise() end); end),
   aw.key({ "Mod4", "Control" }, "a", function (c) aw.client.swap.global_bydirection("left"); gtimer.delayed_call(function () client.focus = c; c:raise() end); c:raise() end),
   aw.key({ "Mod4", "Control" }, "s", function (c) aw.client.swap.global_bydirection("down"); gtimer.delayed_call(function () client.focus = c; c:raise() end); c:raise() end),
   aw.key({ "Mod4", "Control" }, "d", function (c) aw.client.swap.global_bydirection("right"); gtimer.delayed_call(function () client.focus = c; c:raise() end); c:raise() end),

   aw.key({ "Mod4" }, "e", function (c) window_edit_mode.start(c) end),

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
         border_width = 2 * cfg.widget_scale_factor,
         border_color = be.border_normal,
         screen = function(c) return awesome.startup and c.screen or aw.screen.focused() end
      }
   },
   { rule = { instance = "LocalTE" },
     properties = { maximized = true }
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
         focusable = false,
         tag = "STICKY"
      }
   },
   {
      rule = { class = "Wicd-client.py" },
      properties = {
         floating = true
      }
   },
   {
      rule = { class = "Firefox", type = "normal" },
      properties = {
         floating = true,
         maximized = true,
      }
   },
   {
      rule = { class = "Eclipse" },
      properties = { floating = true }
   }
}

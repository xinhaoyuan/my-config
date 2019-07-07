-- awesome v4 config
-- Author: Xinhao Yuan (xinhaoyuan@gmail.com)


-- Remember certain information after client is detached but before the object is gone.
-- Need to do this before everything, so that signal handler fires before standard ones.
client.connect_signal(
   "unmanage",
   function (c)
      c.tomb_floating = c.floating
      c.tomb_class = c.class
      c.tomb_pid = c.pid
   end
)

-- Print the error message to .xsession-errors.
awesome.connect_signal(
   "debug::error",
   function (msg)
      print(msg)
   end
)

local capi = {
   keygrabber = keygrabber,
   client = client,
}
local awful            = require("awful")
local awful_rule       = require("awful.rules")
local awful_layout     = require("awful.layout")
local awful_keygrabber = require("awful.keygrabber")
local naughty          = require("naughty")
local beautiful        = require("beautiful")
local wibox            = require("wibox")
local mouse            = mouse
local gears_timer      = require("gears.timer")
local config           = require("my-config")
local focus            = require("my-focus")
local autofocus        = require("my-autofocus")
local machi            = {
   layout              = require("layout-machi.layout"),
   editor              = require("layout-machi.editor"),
   switcher            = require("layout-machi.switcher")}
local switcher         = require("awesome-switcher-mod")

local HOME_DIR = os.getenv("HOME")

-- Define the tag list upfront for keybindings
local tag_list = { "STICKY", "1", "2", "3", "4" }

-- helper functions

local spawn = function(cmd, to_notify)
   awful.spawn.spawn(cmd)
end

local spawn_with_shell = function(cmd, to_notify)
   awful.spawn.with_shell(cmd)
end

table_join = awful.util.table.join

autofocus.find_alternative_focus = function(prev, s)
   local pid = nil
   if prev and prev.valid then
      pid = prev.tomb_pid or prev.pid
   end

   local filters = {}

   -- prioritize any window has the same pid
   if pid then
      filters[#filters + 1] = function (c)
         return c.valid and c:isvisible() and c.pid == pid
      end
   end

   -- then any visible window
   filters[#filters + 1] = function (c)
      return c.valid and c:isvisible()
   end

   return focus.match_in_history(filters, true)
end

local my_focus_by_direction = function(dir)
   local old_c = capi.client.focus

   if old_c ~= nil and old_c.screen ~= awful.screen.focused() then
      awful.screen.focus(old_c.screen.index)
   end

   awful.client.focus.bydirection(dir);
   local new_c = capi.client.focus

   if new_c == old_c then
      awful.screen.focus_bydirection(dir)
   end

   if capi.client.focus ~= nil then
      capi.client.focus:raise()
   end
end

local manage_mode_enter = function ()

end

-- Alt-Tab switcher

switcher.settings.preview_box = false        -- display preview-box
switcher.settings.cycle_raise_client = true  -- raise clients on cycle

-- execute the initial script

spawn_with_shell(HOME_DIR .. "/.xdesktoprc.awesome", false)

-- keys

local machi_editor = machi.editor.create()

-- base keys and buttons
local global_keys = table_join(
   awful.key({ "Mod1" }, "Tab",
      function ()
         switcher.switch( 1, "Mod1", "Alt_L", "Shift", "Tab")
   end),
   awful.key({ "Mod1", "Shift" }, "Tab",
      function ()
         switcher.switch(-1, "Mod1", "Alt_L", "Shift", "Tab")
   end),
   awful.key({ "Mod4" }, "/",               function () machi_editor.start_interactive() end),
   awful.key({ "Mod4" }, "[",               function () awful_layout.inc(layouts, -1) end),
   awful.key({ "Mod4" }, "]",               function () awful_layout.inc(layouts, 1) end),
   awful.key({ "Mod4" }, "w",               function () my_focus_by_direction("up") end),
   awful.key({ "Mod4" }, "a",               function () my_focus_by_direction("left") end),
   awful.key({ "Mod4" }, "s",               function () my_focus_by_direction("down") end),
   awful.key({ "Mod4" }, "d",               function () my_focus_by_direction("right") end),
   awful.key({ }, "XF86AudioLowerVolume",   function () spawn("amixer sset Master,0 2%-") end),
   awful.key({ }, "XF86AudioRaiseVolume",   function () spawn("amixer sset Master,0 2%+") end),
   awful.key({ }, "XF86AudioMute",          function () spawn("amixer sset Master,0 toggle") end),
   awful.key({ }, "XF86MonBrightnessUp",    function () spawn("xbacklight -inc 5") end),
   awful.key({ }, "XF86MonBrightnessDown",  function () spawn("xbacklight -dec 5") end),
   awful.key({ "Mod4" }, "Return",          function () spawn(config.cmd_terminal) end),
   awful.key({ "Mod4" }, "t",               function () spawn(config.cmd_terminal) end),
   awful.key({ "Mod4" }, "e",               function () spawn(config.cmd_file_manager) end),
   awful.key({ "Mod4" }, "\\",              function () spawn("rofi show -combi-modi window,drun -show combi -modi combi") end),
   awful.key({ "Mod4", "Control" }, "m",    function ()
         for _, c in pairs(capi.client.get()) do c.minimized = false end
   end),
   awful.key({ "Mod4", "Control" }, "Escape", awesome.quit)
)

-- tag 1 is hidden
for i = 2, #tag_list do
   global_keys =
      table_join(
         awful.key({ "Mod4" }, tostring(i - 1), function () awful.screen.focused().tags[i]:view_only() end),
         awful.key({ "Mod4", "Shift" }, tostring(i - 1), function () awful.tag.viewtoggle(awful.screen.focused().tags[i]) end),
         awful.key({ "Mod4", "Mod1" }, tostring(i - 1), function ()
               local c = client.focus
               if c == nil then return end
               awful.client.toggletag(c.screen.tags[i], c)
         end),
         global_keys)
end


local client_keys = table_join(
   awful.key({ "Mod4" }, "Tab", function (c) if not machi.editor.fit_region(c) then machi.switcher.start(c) end end),

   awful.key({ "Mod4" }, "Up", function (c)
         if c.fullscreen then
         elseif c.minimized then
            c.minimized = false
         elseif not c.maximized then
            c.maximized = true
         end
   end),

   awful.key({ "Mod4" }, "Down", function (c)
         if c.fullscreen then
         elseif c.maximized then
            c.maximized = false
         elseif not c.minimized then
            c.minimized = true
         end
   end),

   awful.key({ "Mod4" }, "f", function (c)
         c.fullscreen = not c.fullscreen
   end),

   awful.key({ "Mod4" }, "Right", function (c)
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

local client_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c) capi.client.focus = c; c:raise() end),
   awful.button({ "Mod4" }, 1, awful.mouse.client.move),
   awful.button({ "Mod4" }, 3, awful.mouse.client.resize)
)

-- back to floating before moving

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

-- --- super key handling (still broken)

-- local global_keys_with_super
-- local super_pressed = function ()
--    capi.keygrabber.run(
--       function (mod, key, event)
--          if key == " " then key = "space" end
--          if event == "press" and key ~= "Super_L" then
--             skip_super_once = true
--             capi.keygrabber.stop()
--             root.keys(global_keys)
--             print("pass through " .. key)
--             gears_timer.start_new(
--                0.1,
--                function ()
--                   root.fake_input("key_press", "Super_L")
--                   root.fake_input("key_press", key)
--                   root.fake_input("key_release", key)
--                   root.fake_input("key_release", "Super_L")
--                   gears_timer.start_new(
--                      0.1, function()
--                         print("switch back")
--                         root.keys(global_keys_with_super)
--                         return false
--                   end)
--                   return false
--             end)
--          elseif event == "release" and key == "Super_L" then
--             -- win key released without any key triggered
--             print("Super_L pressed!")
--             capi.keygrabber.stop()
--          end
--    end)
-- end

root.keys(global_keys)

-- rules

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
   if not c.borderless and c.floating and not c.maximized then
      c.border_width = beautiful.border_width
   else
      c.border_width = 0
   end
end

capi.client.connect_signal("manage", reset_border)
capi.client.connect_signal("property::maximized", reset_border)
capi.client.connect_signal("property::floating", reset_border)

awful_rule.rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = false,
         keys = client_keys,
         buttons = client_buttons,
         borderless = false,
         border_color = beautiful.border_normal,
         screen = function(c) return awesome.startup and c.screen or awful.screen.focused() end,
         floating = true,
      }
   },
   {
      rule = { type = "normal" },
      properties = {
         placement = awful.placement.centered + awful.placement.no_overlap + awful.placement.no_offscreen,
      }
   },
   {
      rule = { class = "URxvt" },
      properties = {
         opacity = 0.9,
      },
   },
   {
      rule = { class = "Synapse" },
      properties = {
         border_width = 0,
         borderless = true
      },
   },
   -- {
   --    rule = { class = "Conky" },
   --    properties = {
   --       floating = true,
   --       sticky = true,
   --       ontop = false,
   --       below = true,
   --       focus = false,
   --       border_width = 0,
   --       focusable = false,
   --       tag = "STICKY"
   --    }
   -- },
}

-- tags and layouts

-- local keys_switch_tags = {}
-- local tag_index = 0
-- for i, t in ipairs(tag_list) do
--    if config.tag_filter(t) then
--       tag_index = tag_index + 1
--       if tag_index >= 10 then break end

--       keys_switch_tags = table_join(
--          keys_switch_tags,
--          awful.key({ "Mod4" }, tostring(tag_index), function ()
--                awful.screen.focused().tags[i]:view_only()
--          end),
--          awful.key({ "Mod4", "Control" }, tostring(tag_index),
--             function ()
--                local c = client.focus
--                if c == nil then return end
--                awful.client.toggletag(c.screen.tags[i], c)
--             end
--          ),
--          awful.key({ "Mod4", "Shift" }, tostring(tag_index),
--             function ()
--                for s in screen do
--                   s.tags[i]:view_only()
--                end
--             end
--          )
--       )
--    end
-- end

-- initialize for each screen

awful.screen.connect_for_each_screen(
   function (s)
      local machi_layout = machi.layout.create()
      machi_editor.try_restore_last(machi_layout, s)
      local layouts = {
         machi_layout
      }

      s:connect_signal(
         "property::workarea",
         function (s)
            -- fix machi layout according to the new workarea
            if machi_layout.cmd then
               machi_editor.set_by_cmd(machi_layout, s, machi_layout.cmd)
            end
         end
      )

      for i, t in ipairs(tag_list) do
         awful.tag.add(t, { layout = layouts[1], gap = 0 })
         -- 1 is the hidden tag
      end
      s.tags[2]:view_only()
   end
)

require("my-widgets")

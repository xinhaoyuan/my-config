-- awesome v4 config
-- Author: Xinhao Yuan (xinhaoyuan@gmail.com)

local capi = {
   awesome = awesome,
   screen = screen,
   keygrabber = keygrabber,
   client = client,
   mouse = mouse,
}

-- Remember certain information after client is detached but before the object is gone.
-- Need to do this before everything, so that signal handler fires before standard ones.
capi.client.connect_signal(
   "unmanage",
   function (c)
      c.tomb_floating = c.floating
      c.tomb_class = c.class
      c.tomb_pid = c.pid
   end
)

-- Print the error message to .xsession-errors.
capi.awesome.connect_signal(
   "debug::error",
   function (msg)
      print(msg)
   end
)

local awful    = require("awful")
local HOME_DIR = os.getenv("HOME")

os.execute(HOME_DIR .. "/.xdesktoprc.awesome")

local awful_rule       = require("awful.rules")
local awful_layout     = require("awful.layout")
local awful_keygrabber = require("awful.keygrabber")
local naughty          = require("naughty")
local beautiful        = require("beautiful")
local wibox            = require("wibox")
local gears_timer      = require("gears.timer")
local config           = require("my-config")
local focus            = require("my-focus")
local autofocus        = require("my-autofocus")
local machi            = require("layout-machi")
local yams             = require("yams")
local utils            = require("my-utils")
local menu             = require("my-menu")
-- local mouse_lock       = require("mouse-lock")
local dpi              = require("beautiful.xresources").apply_dpi

local delayed = gears_timer.delayed_call

beautiful.layout_machi = machi.get_icon()
machi.default_editor.set_gap(beautiful.useless_gap * 2, beautiful.useless_gap)
-- revelation.init()

-- Define the tag list upfront for keybindings
local tag_list = { "STICKY", "1", "2", "3", "4" }
awful_layout.layouts = {
   machi.default_layout,
   awful_layout.suit.tile,
   awful_layout.suit.tile.left,
   awful_layout.suit.tile.bottom,
   awful_layout.suit.tile.top,
   awful_layout.suit.fair,
   awful_layout.suit.fair.horizontal,
   awful_layout.suit.spiral,
   awful_layout.suit.spiral.dwindle,
   awful_layout.suit.magnifier,
   awful_layout.suit.corner.nw,
   -- awful_layout.suit.corner.ne,
   -- awful_layout.suit.corner.sw,
   -- awful_layout.suit.corner.se,
}

-- helper functions

local spawn = awful.spawn.spawn
local spawn_with_shell = awful.spawn.with_shell

local open_tmux_session = function(name)
   config.action_terminal({"tmux", "new", "-As", name})
end

table_join = awful.util.table.join

-- autofocus.find_alternative_focus = function(prev, s)
--    local pid = nil
--    if prev and prev.valid then
--       pid = prev.tomb_pid or prev.pid
--    end

--    local filters = {}

--    -- prioritize any window has the same pid
--    if pid then
--       filters[#filters + 1] = function (c)
--          return c.valid and c:isvisible() and c.pid == pid
--       end
--    end

--    -- then any visible window
--    filters[#filters + 1] = function (c)
--       return c.valid and c:isvisible()
--    end

--    return focus.match_in_history(filters, true)
-- end

local function go_by_direction(dir, with_client)
   if with_client then
      local c = capi.client.focus
      awful.screen.focus_bydirection(dir, c.screen)
      c:move_to_screen(mouse.screen.index)
      c:emit_signal("request::activate", "mouse.resize", {raise = true})
   else
      awful.screen.focus_bydirection(dir)
   end
end

-- keys

-- base keys and buttons
local global_keys = table_join(
   awful.key({ "Mod1" }, "Tab",
      function ()
         yams.default.start()
   end),
   -- awful.key({ "Mod4" }, "q",
   --    function ()
   --       revelation({rule={class="Plank"}, is_excluded=true,
   --                   curr_tag_only=true})
   -- end),
   awful.key({ "Mod4" }, "/",               function () machi.default_editor.start_interactive() end),
   awful.key({ "Mod4" }, "[",               function () awful_layout.inc(awful_layout.layouts, -1) end),
   awful.key({ "Mod4" }, "]",               function () awful_layout.inc(awful_layout.layouts, 1) end),
   awful.key({ "Mod4" }, "Up",              function () go_by_direction("up") end),
   awful.key({ "Mod4" }, "Left",            function () go_by_direction("left") end),
   awful.key({ "Mod4" }, "Down",            function () go_by_direction("down") end),
   awful.key({ "Mod4" }, "Right",           function () go_by_direction("right") end),
   awful.key({ "Control", "Mod4" }, "Up",   function () go_by_direction("up", true) end),
   awful.key({ "Control", "Mod4" }, "Left", function () go_by_direction("left", true) end),
   awful.key({ "Control", "Mod4" }, "Down", function () go_by_direction("down", true) end),
   awful.key({ "Control", "Mod4" }, "Right",function () go_by_direction("right", true) end),
   awful.key({ }, "XF86AudioLowerVolume",   function () spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%", false) end),
   awful.key({ }, "XF86AudioRaiseVolume",   function () spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%", false) end),
   awful.key({ }, "XF86AudioMute",          function () spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false) end),
   awful.key({ }, "XF86MonBrightnessUp",    function () spawn("xbacklight -inc 5", false) end),
   awful.key({ }, "XF86MonBrightnessDown",  function () spawn("xbacklight -dec 5", false) end),
   awful.key({ "Mod4" }, "Escape",          function () menu:show() end),
   awful.key({ "Mod4" }, "Return",          function () config.action_terminal() end),
   awful.key({ "Mod4" }, "w",               function () config.action_web_browser() end),
   awful.key({ "Mod4" }, "t",               function () config.action_terminal() end),
   awful.key({ "Mod4" }, "e",               function () config.action_file_manager() end),
   awful.key({ "Mod4" }, "l",               function () config.action_screen_locker() end),
   awful.key({ "Mod4" }, "\\",              function ()
         local cmd = {"rofi", "show",
                      "-combi-modi", "window,drun",
                      "-show", "combi",
                      "-modi", "combi",
                      "-font", beautiful.mono_font or beautiful.font}
         spawn(cmd)
   end),
   awful.key({ "Mod4" }, "F1",              function() open_tmux_session("F1") end),
   awful.key({ "Mod4" }, "F2",              function() open_tmux_session("F2") end),
   awful.key({ "Mod4" }, "F3",              function() open_tmux_session("F3") end),
   awful.key({ "Mod4" }, "F4",              function() open_tmux_session("F4") end),
   awful.key({ "Mod4", "Shift" }, "F12",    function() config.action_app_finder() end),
   -- keep the both ways of showing the desktop, not sure which one is better for now.
   awful.key({ "Mod4" }, "d",               function ()
         local clients = {}
         local has_visible = false
         for _, c in ipairs(capi.client.get()) do
            if c:isvisible() and awful.client.focus.filter(c) then
               c.orig_minimized = c.minimized
               c.minimized = true
               has_visible = true
            end
         end

         if not has_visible then
            clients = {}
            for _, c in ipairs(capi.client.get()) do
               if c.orig_minimized ~= nil then
                  clients[#clients + 1] = c
               end
            end

            -- I thought I should put newer client later. Turned out to be the reversed way.
            table.sort(
               clients,
               function (a, b)
                  return a.focus_timestamp > b.focus_timestamp
               end
            )

            for _, c in ipairs(clients) do
               c.minimized = c.orig_minimized
               c.orig_minimized = nil
            end
         end
   end),
   awful.key({ "Mod4" }, "q",               function ()
         local to_restore = true
         for s in capi.screen do
            if #s.selected_tags > 0 then
               to_restore = false
               s.orig_selected_tags = s.selected_tags
               awful.tag.viewnone(s)
            end
         end

         if not to_restore then return end
         for s in capi.screen do
            if s.orig_selected_tags ~= nil then
               awful.tag.viewmore(s.orig_selected_tags, s)
               s.orig_selected_tags = nil
            end
         end
   end),
   awful.key({ "Mod4", "Control" }, "r",      capi.awesome.restart),
   awful.key({ "Mod4", "Control" }, "Escape", capi.awesome.quit)
)

-- tag 1 is hidden
for i = 2, #tag_list do
   global_keys =
      table_join(
         awful.key({ "Mod4" }, tostring(i - 1), function () awful.screen.focused().tags[i]:view_only() end),
         awful.key({ "Mod4", "Control" }, tostring(i - 1), function () awful.tag.viewtoggle(awful.screen.focused().tags[i]) end),
         awful.key({ "Mod4", "Shift" }, tostring(i - 1), function ()
               local c = capi.client.focus
               if c == nil then return end
               awful.client.toggletag(c.screen.tags[i], c)
         end),
         global_keys)
end

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
         c.fullscreen = not c.fullscreen
   end),

   awful.key({ "Mod4" }, "`", function (c)
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
   awful.button({ "Mod4" }, 1, function (c) awful.mouse.client.move(c) end),
   awful.button({ "Mod4" }, 3, function (c)
         local _, cc = awful.placement.closest_corner(mouse, {parent = c})
         awful.mouse.client.resize(c, cc)
   end)
)

-- back to floating before moving

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
   if not c.borderless and not c.maximized then
      c.border_width = beautiful.border_width
   else
      c.border_width = 0
   end
end

capi.client.connect_signal("manage", reset_border)
capi.client.connect_signal("property::maximized", reset_border)

awful_rule.rules = {
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
      rule = { class = "URxvt" },
      properties = {
         opacity = 0.9,
      },
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

-- initialize for each screen

awful.screen.connect_for_each_screen(
   function (s)
      for i, t in ipairs(tag_list) do
         local tag = awful.tag.add(t, { screen = s, layout = awful_layout.layouts[1], layouts = awful_layout.layouts })
      end
      -- 1 is the hidden tag
      s.tags[2]:view_only()

      -- fix window geometry
      s:connect_signal(
         "property::geometry",
         function (s)
            local clients = {}
            for _, c in ipairs(s.all_clients) do
               if not c.minimized and c.maximized then
                  c.maximized = false
                  table.insert(clients, c)
               end
            end

            delayed(function ()
                  for _, c in ipairs(clients) do
                     c.maximized = true
                  end
            end)
      end)
   end
)

require("my-widgets")

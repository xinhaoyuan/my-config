local capi = {
   awesome = awesome,
   screen = screen,
   mouse = mouse,
   client = client,
   root = root,
}

local shared = require((...):match("(.-)[^%.]+$") .. "shared")
shared.screen = {}

local awful  = require("awful")
local beautiful = require("beautiful")
local watch = require("awful.widget.watch")
local wibox  = require("wibox")
local gtimer  = require("gears.timer")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local gmath = require("gears.math")
local vicious = require("vicious")
local waffle = require("waffle")
local calendar = require("calendar.calendar")
local menu = require("my-menu")
local dpi = require("beautiful.xresources").apply_dpi
local yams = require("yams")
local placeholder = require("placeholder")
local fixed_margin = require("fixed_margin")

-- helper functions

local table_join = awful.util.table.join
local delayed = gtimer.delayed_call

local function open_tmux_session(name)
   shared.action.terminal({"tmux", "new", "-As", name})
end

local function go_by_direction(dir, with_client)
   if with_client then
      local c = capi.client.focus
      awful.screen.focus_bydirection(dir, c.screen)
      c:move_to_screen(capi.mouse.screen.index)
      c:emit_signal("request::activate", "mouse.resize", {raise = true})
   else
      awful.screen.focus_bydirection(dir)
   end
end

-- add machi layout

local machi = require("layout-machi")

beautiful.layout_machi = machi.get_icon()
machi.default_editor.set_gap(beautiful.useless_gap * 2, beautiful.useless_gap * 2)

local alayout = require("awful.layout")
alayout.layouts = {
   machi.default_layout,
   alayout.suit.tile,
   alayout.suit.tile.left,
   alayout.suit.tile.bottom,
   alayout.suit.tile.top,
   alayout.suit.fair,
   alayout.suit.fair.horizontal,
   alayout.suit.spiral,
   alayout.suit.spiral.dwindle,
   alayout.suit.magnifier,
   alayout.suit.corner.nw,
   -- alayout.suit.corner.ne,
   -- alayout.suit.corner.sw,
   -- alayout.suit.corner.se,
}

-- Define the tag list upfront for keybindings

capi.root.buttons(
   awful.util.table.join(
      awful.button({ }, 3, function () menu:show() end),
      capi.root.buttons()
   )
)

local fortune_widget = wibox.widget {
   align = "center",
   forced_height = beautiful.bar_height,
   widget = wibox.widget.textbox
}
watch({"fortune", "-s"}, 120,
   function(widget, stdout)
      local label = " << " .. stdout:gsub("\n", " ") .. " >> "
      widget:set_text(label)
   end,
   fortune_widget)

-- Screen bar

local my_widgets = {}
local my_tray = wibox.widget.systray()
local my_tag_list_buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ "Mod4" }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ "Mod4" }, 3, awful.client.toggletag)
   -- awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   -- awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

local client_menu_selected = nil
local client_menu = awful.menu({
      { "Close", function () client_menu_selected:kill() end },
      { "(Un)maximize", function () client_menu_selected.maximized = not client_menu_selected.maximized end },
})

local my_tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == capi.client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() then
               awful.tag.viewonly(c:tags()[1])
            end
            -- This will also un-minimize
            -- the client, if needed
            capi.client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 2, function (c)
         shared.client.titlebar_toggle(c)
   end),
   awful.button({ }, 3, function (c)
         client_menu_selected = c
         client_menu:show()
   end)
)

local current_screen = nil

-- a basic stable sort
local function sort(l, c)
   local ret = {}
   for i = 1, #l, 1 do
      ret[i] = l[i]
      for j = i, 2, -1 do
         local to_swap
         if c == nil then
            to_swap = ret[j - 1] > ret[j]
         else
            to_swap = c(ret[j], ret[j - 1])
         end
         if to_swap then
            local tmp = ret[j - 1]
            ret[j - 1] = ret[j]
            ret[j] = tmp
         else
            break
         end
      end
   end
   return ret
end

local alt_color_cache = {}
local function alt_color(color)
   if alt_color_cache[color] == nil then
      local comp = table.pack(gcolor.parse_color(color))
      for i = 1, 3 do
         if comp[i] > 0.5 then
            comp[i] = comp[i] - 0.08
         else
            comp[i] = comp[i] + 0.08
         end
      end
      local ret = "#"
      for i = 1, 4 do
         ret = ret .. string.format("%02x", math.min(255, math.floor(comp[i] * 256)))
      end
      alt_color_cache[color] = ret
   end
   return alt_color_cache[color]
end

local function setup_screen(scr)
   local s = scr.index

   scr.mypromptbox = awful.widget.prompt()

   my_widgets[s] = {}
   local tasklist = awful.widget.tasklist {
      screen = s,
      filter = function (c, s)
         if not awful.widget.tasklist.filter.currenttags(c, s) then
            return false
         end
         return not (c:isvisible() and shared.var.hide_clients_with_titlebars and c.has_titlebar)
      end,
      buttons = my_tasklist_buttons,
      style = { font = beautiful.font },
      layout = beautiful.tasklist_layout[beautiful.bar_style],
      source = function ()
         -- Sort clients with their constant ids to make the order stable.
         local cls = awful.widget.tasklist.source.all_clients()
         table.sort(cls, function (a, b) return a.window < b.window end)
         return cls
      end,
      update_function = function (w, b, l, d, objects, args)
         -- not used any more. just for future reference

         -- -- Reorder the clients so that floating clients are on the right side
         -- fl_clients = {}
         -- clients = {}
         -- for i, obj in ipairs(objects) do
         --    if obj.floating or obj.maximized or obj.maximized_horizontal or obj.maximized_vertical then
         --       fl_clients[#fl_clients + 1] = obj
         --    else
         --       clients[#clients + 1] = obj
         --    end
         -- end
         -- for i, obj in ipairs(fl_clients) do
         --    clients[#clients + 1] = obj
         -- end

         -- A hacky way to alternative the colors of tasklist items
         awful.widget.common.list_update(
            w, b,
            function (object, tb)
               local ret = table.pack(l(object, tb))
               -- background is stored in [2]
               if tb.is_odd_child then
                  ret[2] = alt_color(ret[2])
               end
               return table.unpack(ret)
            end,
            d, objects, args)
      end,
      widget_template = beautiful.tasklist_template,
   }
   tasklist = {
      main = tasklist,
      alt = fortune_widget,
      widget = placeholder,
   }
   my_widgets[s].tasklist = tasklist

   my_widgets[s].tag_list = awful.widget.taglist(
      s, function (t) return true end, my_tag_list_buttons,
      {
         font = beautiful.font
      }
   )

   my_widgets[s].wibar = awful.wibar({
         screen = s,
         fg = beautiful.fg_normal,
         bg = beautiful.bar_style == "split" and "#00000000" or beautiful.bg_normal,
         height = beautiful.bar_height + beautiful.border_width,
         position = "bottom",
         border_width = 0,
         cursor = "cross",
   })

   local wc_button = wibox.widget{
      markup = '☯',
      font = beautiful.font,
      widget = wibox.widget.textbox
   }

   my_widgets[s].indicator = wibox.widget {
      {
         wc_button,
         left = dpi(5),
         right = dpi(5),
         widget = wibox.container.margin
      },
      widget = wibox.container.background
   }

   my_widgets[s].indicator:connect_signal(
      "button::press",
      function (_, _, _, b)
         local c = capi.client.focus
         if c == nil then return end
         if b == 1 then
            -- move the mouse to the center of the client before movement
            capi.mouse.coords({
                  x = c.x + c.width / 2,
                  y = c.y + c.height / 2,
                         }, true)
            awful.mouse.client.move(c)
         elseif b == 2 then
            awful.titlebar.toggle(c)
         elseif b == 3 then
            awful.mouse.client.resize(c)
         end
      end
                                         )

   local left_layout = wibox.layout.fixed.horizontal()
   local layoutbox = awful.widget.layoutbox(s)
   layoutbox:buttons(
      awful.util.table.join(
         awful.button({ }, 1, function () waffle:set_gravity("southwest"); waffle:show() end),
         awful.button({ }, 3, function () menu:show() end),
         awful.button({ }, 4, function () awful.layout.inc( 1) end),
         awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   left_layout:add(layoutbox)
   left_layout:add(my_widgets[s].tag_list)
   left_layout:add(scr.mypromptbox)
   local right_layout = wibox.widget {
      spacing        = dpi(5),
      spacing_widget = { color = beautiful.bg_normal, widget = wibox.widget.separator },
      layout         = wibox.layout.fixed.horizontal
   }

   if scr == capi.screen.primary then
      right_layout:add(my_tray)
   end

   -- local volume_widget = wibox.widget.textbox()
   -- volume_widget:set_font(beautiful.font)
   -- vicious.register(volume_widget, vicious.widgets.volume,
   --                  function (widget, args)
   --                     local label = {["♫"] = "O", ["♩"] = "M"}
   --                     return ("V[%d%% %s]"):format(
   --                        args[1], label[args[2]])
   --                  end, 2, "Master")
   -- right_layout:add(volume_widget)
   -- volumearc_widget:set_forced_height(beautiful.bar_height - dpi(4))
   -- volumearc_widget:set_forced_width(beautiful.bar_height - dpi(4))
   -- right_layout:add(volumearc_widget)
   -- right_layout:add(battery_widget)
   local clock = wibox.widget.textclock("%m/%d/%y %a %H:%M")
   clock:set_font(beautiful.font)
   local calendar_widget = calendar({ fdow = 7, html = "<span font_desc='" .. beautiful.font_mono .. "'>\n%s</span>", today_color = beautiful.emphasis_color, position = "bottom_right" })
   calendar_widget:attach(clock)
   right_layout:add(clock)
   right_layout:add(my_widgets[s].indicator)


   local layout

   if beautiful.bar_style == "split" then
      local margin = fixed_margin(wibox.widget {
         {
            tasklist,
            bg = beautiful.bg_normal,
            widget = wibox.container.background,
         },
         top = beautiful.border_width,
         left = beautiful.border_width,
         right = beautiful.border_width,
         color = beautiful.border_focus,
         widget = wibox.container.margin,
      })
      layout = wibox.widget {
         {
            {
               left_layout,
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
            },
            top = beautiful.border_width,
            right = beautiful.border_width,
            color = beautiful.border_focus,
            widget = wibox.container.margin,
         },
         {
            {
               margin,
               widget = wibox.container.place,
            },
            left = dpi(10),
            right = dpi(10),
            widget = wibox.container.margin,
         },
         {
            {
               right_layout,
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
            },
            top = beautiful.border_width,
            left = beautiful.border_width,
            color = beautiful.border_focus,
            widget = wibox.container.margin,
         },
         layout = wibox.layout.align.horizontal,
      }
   else
      layout = wibox.widget {
         {
            left_layout,
            {
               {
                  tasklist,
                  content_fill_horizontal = true,
                  widget = wibox.container.place,
               },
               right = dpi(5),
               widget = wibox.container.margin,
            },
            right_layout,
            layout = wibox.layout.align.horizontal,
         },
         top = beautiful.border_width,
         color = beautiful.border_focus,
         widget = wibox.container.margin,
      }
   end
   my_widgets[s].wibar:set_widget(layout)
end

local function reset_widgets_for_screens()
   for _, w in ipairs(my_widgets) do
      w.wibar:remove()
   end
   my_widgets = {}
   current_screen = nil

   for scr in capi.screen do
      setup_screen(scr)
   end

   shared.action.wallpaper_setup()
end

table.insert(shared.on_start_functions, reset_widgets_for_screens)

capi.screen.connect_signal("list", reset_widgets_for_screens)
capi.screen.connect_signal("primary_changed", reset_widgets_for_screens)

capi.root.keys(
   awful.util.table.join(
      capi.root.keys(),
      awful.key({ "Mod4" }, "F12", function () waffle:set_gravity("center"); waffle:show() end),
      awful.key({ "Mod4" }, ";",
         function ()
            awful.prompt.run {
               prompt       = "Run Lua code: ",
               font         = beautiful.font,
               textbox      = awful.screen.focused().mypromptbox.widget,
               exe_callback = awful.util.eval,
               history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
         end,
         {description = "lua execute prompt", group = "awesome"})
))

gtimer {
   timeout = 0.5,
   autostart = true,
   callback = function()
      local nscreen = capi.mouse.screen.index
      if nscreen ~= current_screen then
         if current_screen ~= nil then
            my_widgets[current_screen].indicator:set_bg(beautiful.bg_normal)
            my_widgets[current_screen].indicator:set_fg(beautiful.fg_normal)
         end
         my_widgets[nscreen].indicator:set_bg(beautiful.bg_focus)
         my_widgets[nscreen].indicator:set_fg(beautiful.fg_focus)
         -- switch active screen
         current_screen = nscreen
      end
   end
}

-- base keys and buttons
local global_keys = table_join(
   awful.key({ "Mod1" }, "Tab",
      function ()
         yams.default.start()
   end),
   awful.key({ "Mod4" }, "/",               function () machi.default_editor.start_interactive() end),
   awful.key({ "Mod4" }, "[",               function () alayout.inc(alayout.layouts, -1) end),
   awful.key({ "Mod4" }, "]",               function () alayout.inc(alayout.layouts, 1) end),
   awful.key({ "Mod4" }, "Up",              function () go_by_direction("up") end),
   awful.key({ "Mod4" }, "Left",            function () go_by_direction("left") end),
   awful.key({ "Mod4" }, "Down",            function () go_by_direction("down") end),
   awful.key({ "Mod4" }, "Right",           function () go_by_direction("right") end),
   awful.key({ "Control", "Mod4" }, "Up",   function () go_by_direction("up", true) end),
   awful.key({ "Control", "Mod4" }, "Left", function () go_by_direction("left", true) end),
   awful.key({ "Control", "Mod4" }, "Down", function () go_by_direction("down", true) end),
   awful.key({ "Control", "Mod4" }, "Right",function () go_by_direction("right", true) end),
   awful.key({ }, "XF86AudioLowerVolume",   function () shared.action.audio_setup("volume-adjust", -5) end),
   awful.key({ }, "XF86AudioRaiseVolume",   function () shared.action.audio_setup("volume-adjust",  5) end),
   awful.key({ }, "XF86AudioMute",          function () shared.action.audio_setup("mute-toggle") end),
   awful.key({ }, "XF86MonBrightnessUp",    function () awful.spawn("xbacklight -inc 5", false) end),
   awful.key({ }, "XF86MonBrightnessDown",  function () awful.spawn("xbacklight -dec 5", false) end),
   awful.key({ "Mod4" }, "Escape",          function () menu:show() end),
   awful.key({ "Mod4" }, "Return",          function () shared.action.terminal() end),
   awful.key({ "Mod4" }, "w",               function () shared.action.web_browser() end),
   awful.key({ "Mod4" }, "e",               function () shared.action.file_manager() end),
   awful.key({ "Mod4" }, "l",               function () shared.action.screen_locker() end),
   awful.key({ "Mod4" }, "\\",              function () shared.action.launcher() end),
   awful.key({ "Mod4", "Shift" }, "F12",    function () shared.action.app_finder() end),
   awful.key({ "Mod4" }, "F1",              function () open_tmux_session("F1") end),
   awful.key({ "Mod4" }, "F2",              function () open_tmux_session("F2") end),
   awful.key({ "Mod4" }, "F3",              function () open_tmux_session("F3") end),
   awful.key({ "Mod4" }, "F4",              function () open_tmux_session("F4") end),
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

-- tags and layouts

shared.screen.tags = { "1", "2", "3", "4" }

for i = 1, #shared.screen.tags do
   local key = tostring(i)
   global_keys =
      table_join(
         awful.key({ "Mod4" }, key, function () awful.screen.focused().tags[i]:view_only() end),
         awful.key({ "Mod4", "Control" }, key, function () awful.tag.viewtoggle(awful.screen.focused().tags[i]) end),
         awful.key({ "Mod4", "Shift" }, key, function ()
               local c = capi.client.focus
               if c == nil then return end
               c:toggle_tag(c.screen.tags[i])
         end),
         global_keys)
end

capi.root.keys(table_join(capi.root.keys(), global_keys))

-- initialize tags for each screen

awful.screen.connect_for_each_screen(
   function (s)
      for i, t in ipairs(shared.screen.tags) do
         local tag = awful.tag.add(t, { screen = s, layout = alayout.layouts[1], layouts = alayout.layouts })
      end

      s.tags[1]:view_only()

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

return nil

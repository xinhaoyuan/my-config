local awful  = require("awful")
local beautiful = require("beautiful")
local watch = require("awful.widget.watch")
local wibox  = require("wibox")
local tm  = require("gears.timer")
local shp = require("gears.shape")
local gmath = require("gears.math")
local vicious = require("vicious")
local waffle = require("waffle")
local config = require("my-config")
-- local volumearc_widget = require("awesome-wm-widgets.volumearc-widget.volumearc")
-- local battery_widget = require("awesome-wm-widgets.battery-widget.battery")
local calendar = require("calendar.calendar")
local menu = require("my-menu")
local dpi = require("beautiful.xresources").apply_dpi

local my_wibar = {}
local my_tag_list = {}
local my_task_list = {}
local my_tray = wibox.widget.systray()
local mono_font = beautiful.mono_font or beautiful.font

root.buttons(
   awful.util.table.join(
      awful.button({ }, 3, function () menu:show() end),
      root.buttons()
   )
)

-- -- dummy bar for conky
-- awful.wibar.new({ position = "top", height = config.bar_height * config.widget_scale_factor, opacity = 0 })

local waffle_width = dpi(200)

local function _simple_button(args)
   local ret = {}
   local action = args.action
   ret.textbox = wibox.widget {
      markup = args.markup or nil,
      text = args.text or nil,
      font = beautiful.fontname_mono .. " 12",
      forced_width = waffle_width,
      align = "center",
      widget = wibox.widget.textbox,
   }
   ret.widget = wibox.widget {
      {
         ret.textbox,
         buttons = action and awful.util.table.join(
            awful.button({ }, 1, nil, function () action(false) end),
            awful.button({ }, 3, nil, function () action(true) end)),
         top = dpi(5), bottom = dpi(5),
         widget = wibox.container.margin,
      },
      fg = beautiful.fg_normal,
      bg = beautiful.bg_normal,
      widget = wibox.container.background
   }

   ret.widget:connect_signal(
      "mouse::enter",
      function ()
         ret.widget.fg = beautiful.fg_focus
         ret.widget.bg = beautiful.bg_focus
      end
   )

   ret.widget:connect_signal(
      "mouse::leave",
      function ()
         ret.widget.fg = beautiful.fg_normal
         ret.widget.bg = beautiful.bg_normal
      end
   )

   if args.key ~= nil and action then
      ret.keys = { args.key }
      ret.key_handler = function (mod, _, event)
         for _, m in ipairs(mod) do
            mod[m] = true
         end
         if event == "press" then
            action(mod["Shift"])
         end
      end
   end

   return ret
end

local function view_with_background_and_border(view)
   local ret = {
      widget = wibox.widget {
         {
            view.widget,
            bg = beautiful.bg_normal,
            fg = beautiful.fg_normal,
            widget = wibox.container.background,
         },
         top = beautiful.border_width,
         bottom = beautiful.border_width,
         left = beautiful.border_width,
         right = beautiful.border_width,
         color = beautiful.border_focus,
         widget = wibox.container.margin,
      },
   }

   setmetatable(ret, { __index = view })
   return ret
end

local waffle_poweroff_count = 0
local waffle_poweroff_button = nil
waffle_poweroff_button = _simple_button({
      markup = "",
      key = "p",
      action = function (alt)
         waffle_poweroff_count = waffle_poweroff_count + 1
         if waffle_poweroff_count >= 2 then
            awful.spawn({"systemctl", "poweroff"})
            waffle:hide()
         else
            waffle_poweroff_button:update_text()
         end
      end
})

function waffle_poweroff_button:update_text()
   self.textbox.markup = "<u>P</u>ower off\n(" .. tostring(2 - waffle_poweroff_count) .. " more times)"
end

local waffle_poweroff_view = view_with_background_and_border(
   waffle.create_view({
         rows = {
            {
               waffle_poweroff_button,
            },
         },
   })
)

local waffle_setting_view = view_with_background_and_border(
   waffle.create_view({
         rows = {
            {
               _simple_button({
                     markup = "<u>S</u>creen layout",
                     key = "s",
                     action = function (alt)
                        if alt then
                           local cmd = {"arandr"}
                           awful.spawn(cmd)
                        else
                           local cmd = {"rofi-screen-layout",
                                        "-font", beautiful.mono_font or beautiful.font
                           }
                           awful.spawn(cmd)
                        end
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "Pulse <u>a</u>udio",
                     key = "a",
                     action = function (alt)
                        local cmd = {"pavucontrol"}
                        awful.spawn(cmd)
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "S<u>u</u>spend",
                     key = "u",
                     action = function (alt)
                        awful.spawn({"systemctl", "suspend"})
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "<u>L</u>ock screen",
                     key = "l",
                     action = function (alt)
                        config.action_screen_locker()
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "<u>P</u>ower off",
                     key = "p",
                     action = function (alt)
                        waffle_poweroff_count = 0
                        waffle_poweroff_button:update_text()
                        waffle:show(waffle_poweroff_view, true)
                     end
               }),
            },
         }
   })
)

local cpu_widget, ram_widget
do
   local cpugraph_widget = wibox.widget {
      max_value = 100,
      background_color = "#00000000",
      forced_width = waffle_width,
      forced_height = dpi(30),
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      color = "linear:0,0:0,22:0,#FF0000:0.3,#FFFF00:0.5,#74aeab"
   }

   cpu_widget = wibox.widget {
      wibox.container.mirror(cpugraph_widget, { horizontal = true }),
      widget = wibox.container.margin
   }

   local total_prev = 0
   local idle_prev = 0

   watch({"grep", "-e", "^cpu ", "/proc/stat"}, 1,
      function(widget, stdout)
         local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
            stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')

         local total = user + nice + system + idle + iowait + irq + softirq + steal

         local diff_idle = idle - idle_prev
         local diff_total = total - total_prev
         local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10

         widget:add_value(diff_usage)

         total_prev = total
         idle_prev = idle
      end,
      cpugraph_widget
   )
end

local waffle_root_view = view_with_background_and_border(
   waffle.create_view({
         rows = {
            {
               {
                  widget = cpu_widget,
               },
            },
            {
               _simple_button({
                     markup = "<u>W</u>eb browser",
                     key = "w",
                     action = function (alt)
                        config.action_web_browser()
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "Fil<u>e</u> manager",
                     key = "e",
                     action = function (alt)
                        config.action_file_manager()
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "<u>T</u>erminal",
                     key = "t",
                     action = function (alt)
                        config.action_terminal()
                        waffle:hide()
                     end
               }),
            },
            {
               _simple_button({
                     markup = "<u>S</u>etting",
                     key = "s",
                     action = function (alt)
                        waffle:show(waffle_setting_view, true)
                     end
               }),
            },
         }
   })
)

my_tag_list.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ "Mod4" }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ "Mod4" }, 3, awful.client.toggletag)
   -- awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   -- awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

my_task_list.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
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
            client.focus = c
            c:raise()
         end
   end)
   -- awful.button({ }, 3, function ()
   --       if instance then
   --          instance:hide()
   --          instance = nil
   --       else
   --          instance = awful.menu.clients({ width=500 })
   --       end
   -- end),
   -- awful.button({ }, 4, function ()
   --       awful.client.focus.byidx(1)
   --       if client.focus then client.focus:raise() end
   -- end),
   -- awful.button({ }, 5, function ()
   --       awful.client.focus.byidx(-1)
   --       if client.focus then client.focus:raise() end
   -- end)
)

local wc_button_container = {}
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

awful.screen.connect_for_each_screen(function (scr)
      local s = scr.index

      scr.mypromptbox = awful.widget.prompt()

      my_task_list[s] = awful.widget.tasklist {
         screen = s,
         filter = awful.widget.tasklist.filter.currenttags,
         buttons = my_task_list.buttons,
         style = { font = mono_font },
         layout = {
            spacing_widget = {
               {
                  forced_height = dpi(12),
                  thickness = 1,
                  widget = wibox.widget.separator,
               },
               valign = "center",
               halign = "center",
               widget = wibox.container.place,
            },
            spacing = dpi(6),
            layout = wibox.layout.flex.horizontal
         },
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

            awful.widget.common.list_update(w, b, l, d, objects, args)
         end,
         widget_template = beautiful.tasklist_template,
      }

      my_tag_list[s] = awful.widget.taglist(
         s, function (t) return config.tag_filter(t.name) end, my_tag_list.buttons,
         {
            font = mono_font
         }
      )

      my_wibar[s] = awful.wibar({
            screen = s,
            fg = beautiful.fg_normal,
            bg = beautiful.bg_normal,
            height = beautiful.bar_height,
            position = "bottom",
            border_width = 0,
      })

      local wc_button = wibox.widget{
         markup = '☯',
         font = beautiful.font,
         widget = wibox.widget.textbox
      }

      wc_button_container[s] = wibox.widget {
         {
            wc_button,
            left = dpi(5),
            right = dpi(5),
            widget = wibox.container.margin
         },
         widget = wibox.container.background
      }

      wc_button_container[s]:connect_signal(
         "button::press",
         function (_, _, _, b)
            local c = client.focus
            if c == nil then return end
            if b == 1 then
               -- move the mouse to the center of the client before movement
               mouse.coords({
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
            awful.button({ }, 1, function () waffle:set_gravity("southwest"); waffle:show(waffle_root_view) end),
            awful.button({ }, 3, function () menu:show() end),
            awful.button({ }, 4, function () awful.layout.inc( 1) end),
            awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      left_layout:add(layoutbox)
      left_layout:add(my_tag_list[s])
      left_layout:add(scr.mypromptbox)
      local right_layout = wibox.widget {
         spacing        = dpi(5),
         spacing_widget = { color = beautiful.bg_normal, widget = wibox.widget.separator },
         layout         = wibox.layout.fixed.horizontal
      }

      local tray_padding = dpi(1)
      if tray_padding > 0 then
         my_tray:set_base_size(beautiful.bar_height - tray_padding * 2)
         local tray_layout = wibox.widget {
            { forced_width = 0, forced_height = tray_padding, color = beautiful.bg_normal, widget = wibox.widget.separator },
            my_tray,
            { forced_width = 0, forced_height = tray_padding, color = beautiful.bg_normal, widget = wibox.widget.separator },
            layout = wibox.layout.align.vertical,
         }
         right_layout:add(tray_layout)
      else
         right_layout:add(my_tray)
      end

      -- local volume_widget = wibox.widget.textbox()
      -- volume_widget:set_font(mono_font)
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
      local clock = wibox.widget.textclock(" %m/%d/%y %a %H:%M ")
      clock:set_font(mono_font)
      calendar_widget = calendar({ fdow = 7, position = "bottom_right" })
      calendar_widget:attach(clock)
      right_layout:add(clock)
      right_layout:add(wc_button_container[s])

      local layout = wibox.widget {
         left_layout,
         {
            my_task_list[s],
            left = dpi(5), right = dpi(5),
            widget = wibox.container.margin
         },
         right_layout,
         layout = wibox.layout.align.horizontal,
      }
      my_wibar[s]:set_widget(layout)
end)

root.keys(
   awful.util.table.join(
      root.keys(),
      awful.key({ "Mod4" }, "F12", function () waffle:set_gravity("center"); waffle:show(waffle_root_view) end),
      awful.key({ "Mod4" }, ";",
         function ()
            awful.prompt.run {
               prompt       = "Run Lua code: ",
               font         = beautiful.mono_font or beautiful.font,
               textbox      = awful.screen.focused().mypromptbox.widget,
               exe_callback = awful.util.eval,
               history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
         end,
         {description = "lua execute prompt", group = "awesome"})
))

tm {
   timeout = 0.5,
   autostart = true,
   callback = function()
      local nscreen = mouse.screen.index
      if nscreen ~= current_screen then
         if current_screen ~= nil then
            wc_button_container[current_screen]:set_bg(beautiful.bg_normal)
            wc_button_container[current_screen]:set_fg(beautiful.fg_normal)
         end
         wc_button_container[nscreen]:set_bg(beautiful.bg_focus)
         wc_button_container[nscreen]:set_fg(beautiful.fg_focus)
         -- switch active screen
         current_screen = nscreen
      end
   end
}

client.connect_signal(
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

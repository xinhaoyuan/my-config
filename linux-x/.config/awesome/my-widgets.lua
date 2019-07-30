local aw  = require("awful")
local awc = require("awful.widget.common")
local beautiful = require("beautiful")
local wi  = require("wibox")
local tm  = require("gears.timer")
local shp = require("gears.shape")
local gmath = require("gears.math")
local vicious = require("vicious")
local config = require("my-config")
local volumearc_widget = require("awesome-wm-widgets.volumearc-widget.volumearc")
-- local battery_widget = require("awesome-wm-widgets.battery-widget.battery")
local calendar = require("calendar.calendar")
local freedesktop = require("freedesktop")
local dpi = require("beautiful.xresources").apply_dpi

local my_wibar = {}
local my_tag_list = {}
local my_task_list = {}
local my_tray = wi.widget.systray()
local mono_font = beautiful.mono_font or beautiful.font

local fd_menu  = freedesktop.menu.build({
      before = { { "awesome",
                   {
                      { "restart", awesome.restart },
                      { "quit", awesome.quit },
                      { "shutdown", function () os.execute("systemctl poweroff") end },
                   },
                   beautiful.awesome_icon } },
      after = { },
})

root.buttons(
   aw.util.table.join(
      aw.button({ }, 3, function () fd_menu:show() end),
      root.buttons()
   )
)

-- -- dummy bar for conky
-- aw.wibar.new({ position = "top", height = config.bar_height * config.widget_scale_factor, opacity = 0 })

my_tag_list.buttons = aw.util.table.join(
   aw.button({ }, 1, aw.tag.viewonly),
   aw.button({ "Mod4" }, 1, aw.client.movetotag),
   aw.button({ }, 3, aw.tag.viewtoggle),
   aw.button({ "Mod4" }, 3, aw.client.toggletag)
   -- aw.button({ }, 4, function(t) aw.tag.viewnext(aw.tag.getscreen(t)) end),
   -- aw.button({ }, 5, function(t) aw.tag.viewprev(aw.tag.getscreen(t)) end)
)

my_task_list.buttons = aw.util.table.join(
   aw.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() then
               aw.tag.viewonly(c:tags()[1])
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end)
   -- aw.button({ }, 3, function ()
   --       if instance then
   --          instance:hide()
   --          instance = nil
   --       else
   --          instance = aw.menu.clients({ width=500 })
   --       end
   -- end),
   -- aw.button({ }, 4, function ()
   --       aw.client.focus.byidx(1)
   --       if client.focus then client.focus:raise() end
   -- end),
   -- aw.button({ }, 5, function ()
   --       aw.client.focus.byidx(-1)
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

aw.screen.connect_for_each_screen(function (scr)
      local s = scr.index

      my_task_list[s] = aw.widget.tasklist {
         screen = s,
         filter = aw.widget.tasklist.filter.currenttags,
         buttons = my_task_list.buttons,
         style = {
            font = mono_font
         },
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

            awc.list_update(w, b, l, d, objects, args)
         end,
         widget_template = config.tasklist_template,
      }

      my_tag_list[s] = aw.widget.taglist(
         s, function (t) return config.tag_filter(t.name) end, my_tag_list.buttons,
         {
            font = mono_font
         }
      )

      my_wibar[s] = aw.wibar({
            screen = s,
            fg = beautiful.fg_normal,
            bg = beautiful.bg_normal,
            height = beautiful.bar_height,
            position = "bottom",
            border_width = 0,
      })

      local wc_button = wi.widget{
         markup = '☯',
         font = beautiful.font,
         widget = wi.widget.textbox
      }

      wc_button_container[s] = wi.widget {
         {
            wc_button,
            left = dpi(5),
            right = dpi(5),
            widget = wi.container.margin
         },
         widget = wi.container.background
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
               aw.mouse.client.move(c)
            elseif b == 3 then
               aw.mouse.client.resize(c)
            end
         end
      )

      local left_layout = wi.layout.fixed.horizontal()
      local layoutbox = aw.widget.layoutbox(s)
      layoutbox:buttons(
         aw.util.table.join(
            aw.button({ }, 3, function () fd_menu:show() end),
            aw.button({ }, 4, function () aw.layout.inc( 1) end),
            aw.button({ }, 5, function () aw.layout.inc(-1) end)))
      left_layout:add(layoutbox)
      left_layout:add(my_tag_list[s])
      local right_layout = wi.widget {
         spacing        = dpi(5),
         spacing_widget = { color = beautiful.bg_normal, widget = wi.widget.separator },
         layout         = wi.layout.fixed.horizontal
      }

      local tray_padding = dpi(1)
      if tray_padding > 0 then
         my_tray:set_base_size(beautiful.bar_height - tray_padding * 2)
         local tray_layout = wi.widget {
            { forced_width = 0, forced_height = tray_padding, color = beautiful.bg_normal, widget = wi.widget.separator },
            my_tray,
            { forced_width = 0, forced_height = tray_padding, color = beautiful.bg_normal, widget = wi.widget.separator },
            layout = wi.layout.align.vertical,
         }
         right_layout:add(tray_layout)
      else
         right_layout:add(my_tray)
      end

      -- local volume_widget = wi.widget.textbox()
      -- volume_widget:set_font(mono_font)
      -- vicious.register(volume_widget, vicious.widgets.volume,
      --                  function (widget, args)
      --                     local label = {["♫"] = "O", ["♩"] = "M"}
      --                     return ("V[%d%% %s]"):format(
      --                        args[1], label[args[2]])
      --                  end, 2, "Master")
      -- right_layout:add(volume_widget)
      volumearc_widget:set_forced_height(beautiful.bar_height - dpi(4))
      volumearc_widget:set_forced_width(beautiful.bar_height - dpi(4))
      right_layout:add(volumearc_widget)
      -- right_layout:add(battery_widget)
      local clock = wi.widget.textclock(" %m/%d/%y %a %H:%M ")
      clock:set_font(mono_font)
      calendar_widget = calendar({ fdow = 7, position = "bottom_right" })
      calendar_widget:attach(clock)
      right_layout:add(clock)
      right_layout:add(wc_button_container[s])

      local layout = wi.layout.align.horizontal()
      layout:set_left(left_layout)
      layout:set_middle(my_task_list[s])
      layout:set_right(right_layout)

      my_wibar[s]:set_widget(layout)
end)

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

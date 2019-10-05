local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local capi = {
   client = client,
   screen = screen,
}

local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local waffle = require("waffle")
local watch = require("awful.widget.watch")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local icons = require("icons")
local highlighted_textbox = require("highlighted_textbox")
local dpi = require("beautiful.xresources").apply_dpi

local waffle_width = beautiful.waffle_width or dpi(240)
local button_height = dpi(20)
local button_padding = dpi(4)
local font_big = beautiful.fontname_normal .. " 10"
local font_small = beautiful.fontname_normal .. " 7"
local update_interval_s = 1

local function em(t)
   return "<span color='" .. beautiful.emphasis_color .. "'>" .. t .. "</span>"
end

local function create_view(root)
   local checked = {[root] = true}
   local traverse_pool = {root}
   local view = {}
   view.keys = {}

   -- traverse the hierachy in depth-first order
   while #traverse_pool > 0 do
      local widget = traverse_pool[#traverse_pool]
      table.remove(traverse_pool, #traverse_pool)

      for _, c in ipairs(widget:get_children()) do
         if not checked[c] then
            checked[c] = true
            table.insert(traverse_pool, c)
         end
      end

      if widget.keys then
         for k, f in pairs(widget.keys) do
            if not view.keys[k] then
               view.keys[k] = f
            end
         end
      end
   end

   view.widget = root
   view.key_handler = function (mod, key, event)
      if event == "press" and view.keys[key] then
         view.keys[key](mod, key, event)
         return true
      else
         return false
      end
   end

   view.options = {
      -- Options here.
   }

   return view
end

local function simple_button(args)
   local action = args.action
   local width = waffle_width - button_padding * 2

   local textbox = wibox.widget {
      markup = args.markup or nil,
      text = args.text or nil,
      font = font_big,
      forced_height = button_height,
      align = "center",
      valign = "center",
      widget = wibox.widget.textbox,
   }

   local ret = wibox.widget {
      {
         {
            args.icon and {
               image = args.icon,
               resize = true,
               forced_width = button_height,
               forced_height = button_height,
               widget = wibox.widget.imagebox,
                          },
            textbox,
            args.indicator and {
               markup = args.indicator,
               font = font_big,
               forced_height = button_height,
               align = "center",
               valign = "center",
               widget = wibox.widget.textbox,
            },
            forced_width = width,
            layout = wibox.layout.align.horizontal,
         },
         buttons = action and awful.util.table.join(
            awful.button({ }, 1, nil, function () action(false) end),
            awful.button({ }, 3, nil, function () action(true) end)),
         margins = button_padding,
         widget = wibox.container.margin,
      },
      fg = beautiful.fg_normal,
      bg = beautiful.bg_normal,
      widget = wibox.container.background
   }

   ret:connect_signal(
      "mouse::enter",
      function ()
         ret.fg = beautiful.fg_focus
         ret.bg = beautiful.bg_focus
      end
   )

   ret:connect_signal(
      "mouse::leave",
      function ()
         ret.fg = beautiful.fg_normal
         ret.bg = beautiful.bg_normal
      end
   )

   if args.key ~= nil and action then
      local function cb(mod, _, event)
         for _, m in ipairs(mod) do
            mod[m] = true
         end
         if event == "press" then
            action(mod["Shift"])
         end
      end
      ret.keys = {}
      if type(args.key) == "table" then
         for _, k in ipairs(args.key) do
            ret.keys[k] = cb
         end
      else
         ret.keys[args.key] = cb
      end
   end

   ret.textbox = textbox

   return ret
end

local function decorate(widget)
   return wibox.widget {
      {
         widget,
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
   }
end

local waffle_poweroff_count = 0
local waffle_poweroff_button = nil
waffle_poweroff_button = simple_button({
      markup = "",
      indicator = em("p"),
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
   self.textbox.markup = "Power off <span size='x-small'>(" .. tostring(2 - waffle_poweroff_count) .. " more times)</span>"
end

local waffle_poweroff_view = create_view(decorate(waffle_poweroff_button))
local waffle_settings_view = create_view(
   decorate(
      wibox.widget {
         simple_button({
               markup = "Toggle titlebars",
               indicator = em("t"),
               key = "t",
               action = function (alt)
                  if not alt then
                     if shared.var.enable_titlebar then
                        for _, c in ipairs(capi.client.get()) do
                           shared.client.titlebar_disable(c)
                        end
                        shared.var.enable_titlebar = false
                     else
                        for _, c in ipairs(capi.client.get()) do
                           shared.client.titlebar_enable(c)
                        end
                        shared.var.enable_titlebar = true
                     end
                  else
                     shared.var.hide_clients_with_titlebars = not shared.var.hide_clients_with_titlebars
                     capi.client.emit_signal("list")
                  end
                  waffle:hide()
               end
         }),
         simple_button({
               markup = "Toggle fortune",
               indicator = em("f"),
               key = "f",
               action = function (alt)
                  shared.screen.toggle_fortune()
                  waffle:hide()
               end
         }),
         simple_button({
               markup = "Cycle bar styles",
               indicator = em("b"),
               key = "b",
               action = function (alt)
                  waffle:hide()
                  for i, v in ipairs(beautiful.bar_styles) do
                     if v == beautiful.bar_style then
                        beautiful.bar_style = beautiful.bar_styles[i % #beautiful.bar_styles + 1]
                        capi.screen.emit_signal("list")
                        return
                     end
                  end
                  beautiful.bar_style = "simple"
                  capi.screen.emit_signal("list")
               end
         }),
         simple_button({
               markup = "Wallpaper",
               indicator = em("w"),
               key = "w",
               action = function (alt)
                  shared.action.wallpaper_setup()
                  waffle:hide()
               end
         }),
         simple_button({
               markup = "Screen layout",
               indicator = em("s"),
               key = "s",
               action = function (alt)
                  if alt then
                     local cmd = {"arandr"}
                     awful.spawn(cmd)
                  else
                     local cmd = {"rofi-screen-layout",
                                  "-font", beautiful.font
                     }
                     awful.spawn(cmd)
                  end
                  waffle:hide()
               end
         }),
         simple_button({
               markup = "Pulse audio",
               indicator = em("a"),
               key = "a",
               action = function (alt)
                  local cmd = {"pavucontrol"}
                  awful.spawn(cmd)
                  waffle:hide()
               end
         }),
         layout = wibox.layout.fixed.vertical,
   })
)

local cpu_widget_width = waffle_width / 2 - dpi(24)
local cpu_widget
do
   local cpugraph_widget = wibox.widget {
      max_value = 100,
      background_color = "#00000000",
      forced_width = cpu_widget_width,
      forced_height = dpi(24),
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      color = "linear:0,0:0,22:0,#FF0000:0.3,#FFFF00:0.5,#74aeab"
   }


   local cpu_text_widget = highlighted_textbox(
      wibox.widget {
         forced_width = cpu_widget_width,
         forced_height = dpi(24),
         align = "center",
         point = {x = 0, y = 0},
         widget = wibox.widget.textbox,
      }, beautiful.bg_normal, dpi(2))

   cpu_widget = wibox.widget {
      {
         cpugraph_widget,
         cpu_text_widget,
         layout = wibox.layout.manual
      },
      width = cpu_widget_width,
      height = dpi(24),
      widget = wibox.container.constraint,
   }

   local total_prev = 0
   local idle_prev = 0

   watch({"grep", "-e", "^cpu ", "/proc/stat"}, update_interval_s,
      function(widget, stdout)
         local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
            stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')

         local total = user + nice + system + idle + iowait + irq + softirq + steal

         local diff_idle = idle - idle_prev
         local diff_total = total - total_prev
         local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10

         local markup = "<span font_desc='" .. font_small .. "'>" .. tostring(math.floor(diff_usage)) .. "%</span>"
         cpu_text_widget:set_markup(markup)

         cpugraph_widget:add_value(diff_usage)

         total_prev = total
         idle_prev = idle
      end,
      cpu_widget
   )
end

local function format_size(s)
   local unit = ""
   if s >= 1000 * 1000 * 1000 then
      s = math.floor(s / 1000 / 1000 / 1000 * 100) / 100
      unit = "g"
   elseif s >= 1000 * 1000 then
      s = math.floor(s / 1000 / 1000 * 100) / 100
      unit = "m"
   elseif s >= 1000 then
      s = math.floor(s / 1000 * 100) / 100
      unit = "k"
   else
      s = math.floor(s * 100) / 100
   end
   return tostring(s) .. unit
end

local ram_widget_width = waffle_width / 2 - dpi(24)
local ram_widget
do
   local ramgraph_widget = wibox.widget {
      max_value = 100,
      background_color = "#00000000",
      forced_width = ram_widget_width,
      forced_height = dpi(24),
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      color = "linear:0,0:0,22:0,#FF0000:0.3,#FFFF00:0.5,#74aeab"
   }

   local ram_text_widget = highlighted_textbox(
      wibox.widget {
         forced_width = ram_widget_width,
         forced_height = dpi(24),
         align = "center",
         point = {x = 0, y = 0},
         widget = wibox.widget.textbox,
      }, beautiful.bg_normal, dpi(2))

   ram_widget = wibox.widget {
      {
         ramgraph_widget,
         ram_text_widget,
         layout = wibox.layout.manual
      },
      width = ram_widget_width,
      height = dpi(24),
      widget = wibox.container.constraint,
   }

   watch({"egrep", "-e", "MemTotal:|MemAvailable:", "/proc/meminfo"}, update_interval_s,
      function(widget, stdout, stderr, exitreason, exitcode)
         local total, available = stdout:match('MemTotal:%s+([0-9]+) .*MemAvailable:%s+([0-9]+)')
         local usage = math.floor((total - available) / total * 100 + 0.5)

         local markup = "<span font_desc='" .. font_small .. "'>" .. format_size((total - available) * 1000) .. "B</span>"
         ram_text_widget:set_markup(markup)

         ramgraph_widget:add_value(usage)
      end,
      ram_widget
   )
end

local net_widget_width = (waffle_width - dpi(24)) / 2
local net_widget
do
   local netgraph_rx_widget = wibox.widget {
      background_color = "#00000000",
      forced_width = net_widget_width,
      forced_height = dpi(24),
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      -- scale = true,
      color = "#74aeab"
   }

   local rx_text_widget = highlighted_textbox(
      wibox.widget {
         forced_width = net_widget_width,
         forced_height = dpi(24),
         align = "center",
         point = {x = 0, y = 0},
         widget = wibox.widget.textbox,
      }, beautiful.bg_normal, dpi(2))

   local net_rx_widget = wibox.widget {
      netgraph_rx_widget,
      point = {x = 0, y = 0},
      widget = wibox.container.margin
   }

   local net_rx_layout = wibox.widget {
      {
         net_rx_widget,
         rx_text_widget,
         layout = wibox.layout.manual,
      },
      width = net_widget_width,
      height = dpi(24),
      widget = wibox.container.constraint,
   }

   local netgraph_tx_widget = wibox.widget {
      background_color = "#00000000",
      forced_width = net_widget_width,
      forced_height = dpi(24),
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      -- scale = true,
      color = "#74aeab"
   }

   local tx_text_widget = highlighted_textbox(
      wibox.widget {
         forced_width = net_widget_width,
         forced_height = dpi(24),
         align = "center",
         point = { x = 0, y = 0 },
         widget = wibox.widget.textbox,
      }, beautiful.bg_normal, dpi(2))

   local net_tx_widget = wibox.widget {
      netgraph_tx_widget,
      widget = wibox.container.margin
   }

   local net_tx_layout = wibox.widget {
      {
         net_tx_widget,
         tx_text_widget,
         layout = wibox.layout.manual,
      },
      width = net_widget_width,
      height = dpi(24),
      widget = wibox.container.constraint,
   }

   net_widget = wibox.widget {
      {
         {
            image = gcolor.recolor_image(icons.ethernet, beautiful.fg_normal),
            forced_height = dpi(16),
            forced_width = dpi(16),
            widget = wibox.widget.imagebox,
         },
         margins = dpi(4),
         widget = wibox.container.margin,
      },
      net_rx_layout,
      net_tx_layout,
      layout = wibox.layout.fixed.horizontal,
   }

   local prev_recv = nil
   local prev_send = nil

   watch({"egrep", "-e", "[[:alnum:]]+:", "/proc/net/dev"}, update_interval_s,
      function(widget, stdout, stderr, exitreason, exitcode)
         local recv = 0
         local send = 0
         for line in stdout:gmatch("[^\r\n]+") do
            local items = {}
            for item in line:gmatch("[^ \t:]+") do
               if #item > 0 then
                  table.insert(items, item)
               end
            end
            if items[1]:match("^tun[0-9]*$") == nil then
               -- Skips VPN for avoiding double-couting
               recv = recv + tonumber(items[2])
               send = send + tonumber(items[10])
            end
         end

         if prev_recv ~= nil then
            local rx = (recv - prev_recv) / update_interval_s
            local markup = "<span font_desc='" .. font_small .. "'>RX " .. format_size(rx) .. "B/s</span>"
            rx_text_widget:set_markup(markup)
            netgraph_rx_widget.max_value = 256 * 1024
            netgraph_rx_widget:add_value(rx)
         end
         prev_recv = recv
         if prev_send ~= nil then
            local tx = (send - prev_send) / update_interval_s
            local markup = "<span font_desc='" .. font_small .. "'>TX " .. format_size(tx) .. "B/s</span>"
            tx_text_widget:set_markup(markup)
            netgraph_tx_widget.max_value = 256 * 1024
            netgraph_tx_widget:add_value(tx)
         end
         prev_send = send
      end,
      netgraph_widget
   )
end

local volumebar_widget_width = waffle_width - dpi(24)
local volumebar_widget
do
   local GET_VOLUME_CMD = 'amixer -D pulse sget Master'
   local INC_VOLUME_CMD = 'amixer -D pulse sset Master 5%+'
   local DEC_VOLUME_CMD = 'amixer -D pulse sset Master 5%-'
   local TOG_VOLUME_CMD = 'amixer -D pulse sset Master toggle'

   local bar_color = "#74aeab"
   local mute_color = "#ff0000"
   local background_color = "#3a3a3a"

   volumebar_widget = wibox.widget {
      max_value = 1,
      forced_width = volumebar_widget_width,
      forced_height = dpi(24),
      paddings = 0,
      border_width = 0.5,
      color = bar_color,
      background_color = background_color,
      shape = gshape.bar,
      clip = true,
      margins = {
         right = dpi(4),
         top = dpi(11),
         bottom = dpi(11),
      },
      widget = wibox.widget.progressbar
   }

   local update_graphic = function (widget, stdout, _, _, _)
      local mute = string.match(stdout, "%[(o%D%D?)%]")
      local volume = string.match(stdout, "(%d?%d?%d)%%")
      volume = tonumber(string.format("% 3d", volume))

      widget.value = volume / 100;
      widget.color = mute == "off" and mute_color
         or bar_color

   end

   volumebar_widget:connect_signal(
      "button::press",
      function(_, _, _, button)
         local cmd
         if (button == 4)     then cmd = INC_VOLUME_CMD
         elseif (button == 5) then cmd = DEC_VOLUME_CMD
         elseif (button == 1) then cmd = TOG_VOLUME_CMD
         end

         awful.spawn.easy_async_with_shell(
            cmd .. ">/dev/null&&" .. GET_VOLUME_CMD,
            function (stdout, stderr, exitreason, exitcode)
               update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
            end
         )
      end
   )

   watch(GET_VOLUME_CMD, 1, update_graphic, volumebar_widget)

   volumebar_widget.keys = {
      ["-"] = function (mod, _, event)
         awful.spawn.easy_async_with_shell(
            DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
            function (stdout, stderr, exitreason, exitcode)
               update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
            end
         )
      end,
      ["_"] = function ()
         awful.spawn.easy_async_with_shell(
            TOG_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
            function (stdout, stderr, exitreason, exitcode)
               update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
            end
         )
      end,
      ["="] = function ()
         awful.spawn.easy_async_with_shell(
            INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
            function (stdout, stderr, exitreason, exitcode)
               update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
            end
         )
      end,
   }
end

local waffle_root_view = create_view(
   wibox.widget {
      -- highlighted_textbox(
      --    wibox.widget {
      --       align = "center",
      --       format = "<span size='x-large'>%y-%m-%d %a %H:%M</span>",
      --       widget = wibox.widget.textclock,
      --    },
      --    beautiful.bg_normal, dpi(2)
      -- ),
      decorate(
         wibox.widget {
            {
               {
                  {
                     {
                        image = gcolor.recolor_image(icons.cpu, beautiful.fg_normal),
                        forced_height = dpi(16),
                        forced_width = dpi(16),
                        widget = wibox.widget.imagebox,
                     },
                     margins = dpi(4),
                     widget = wibox.container.margin,
                  },
                  cpu_widget,
                  {
                     {
                        image = gcolor.recolor_image(icons.ram, beautiful.fg_normal),
                        forced_height = dpi(16),
                        forced_width = dpi(16),
                        widget = wibox.widget.imagebox,
                     },
                     margins = dpi(4),
                     widget = wibox.container.margin,
                  },
                  ram_widget,
                  layout = wibox.layout.fixed.horizontal,
               },
               net_widget,
               spacing = dpi(2),
               layout = wibox.layout.fixed.vertical,
            },
            bg = beautiful.bg_normal,
            widget = wibox.container.background,
      }),
      -- decorate(
      --    wibox.widget {
      --       wibox.widget.calendar.month(os.date('*t'), beautiful.font_mono),
      --       top = dpi(10), right = dpi(10),
      --       widget = wibox.container.margin,
      --    }
      -- ),
      decorate(
         wibox.widget {
            simple_button({
                  icon = gcolor.recolor_image(icons.launcher, beautiful.fg_normal),
                  markup = "Launcher",
                  indicator = em("\\"),
                  key = {"\\","|"},
                  action = function (alt)
                     if alt then
                        shared.action.app_finder()
                     else
                        shared.action.launcher()
                     end
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.terminal, beautiful.fg_normal),
                  markup = "Terminal",
                  indicator = em("↵"),
                  key = "Return",
                  action = function (alt)
                     if alt then
                        awful.spawn("xterm")
                     else
                        shared.action.terminal()
                     end
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.browser, beautiful.fg_normal),
                  markup = "Web browser",
                  indicator = em("w"),
                  key = "w",
                  action = function (alt)
                     shared.action.web_browser()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.file_manager, beautiful.fg_normal),
                  markup = "File manager",
                  indicator = em("e"),
                  key = "e",
                  action = function (alt)
                     shared.action.file_manager()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.music, beautiful.fg_normal),
                  markup = "Music",
                  indicator = em("m"),
                  key = "m",
                  action = function (alt)
                     shared.action.music_app()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.fortune, beautiful.fg_normal),
                  markup = "Fortune",
                  indicator = em("f"),
                  key = "f",
                  action = function (alt)
                     local fortune = shared.screen.get_fortune()
                     if fortune then
                        shared.action.web_browser("? " .. fortune)
                     end
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.setup, beautiful.fg_normal),
                  markup = "Settings",
                  indicator = em("s"),
                  key = "s",
                  action = function (alt)
                     waffle:show(waffle_settings_view, true)
                  end
            }),
            layout = wibox.layout.fixed.vertical,
      }),
      decorate(
         wibox.widget {
            {
               {
                  {
                     {
                        image = gcolor.recolor_image(icons.audio, beautiful.fg_normal),
                        forced_height = dpi(16),
                        forced_width = dpi(16),
                        widget = wibox.widget.imagebox,
                     },
                     margins = dpi(4),
                     widget = wibox.container.margin,
                  },
                  volumebar_widget,
                  layout = wibox.layout.fixed.horizontal,
               },
               layout = wibox.layout.fixed.vertical,
            },
            bg = beautiful.bg_normal,
            widget = wibox.container.background,
      }),
      decorate(
         wibox.widget {
            simple_button({
                  icon = gcolor.recolor_image(icons.lock, beautiful.fg_normal),
                  markup = "Lock screen",
                  indicator = em("l"),
                  key = "l",
                  action = function (alt)
                     shared.action.screen_locker()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.sleep, beautiful.fg_normal),
                  markup = "Suspend",
                  indicator = em("u"),
                  key = "u",
                  action = function (alt)
                     awful.spawn({"systemctl", "suspend"})
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.poweroff, beautiful.fg_normal),
                  markup = "Power off",
                  indicator = em("p"),
                  key = "p",
                  action = function (alt)
                     waffle_poweroff_count = 0
                     waffle_poweroff_button:update_text()
                     waffle:show(waffle_poweroff_view, true)
                  end
            }),
            layout = wibox.layout.fixed.vertical,
      }),
      spacing = dpi(10),
      layout = wibox.layout.fixed.vertical,
   }
)

waffle:set_root_view(waffle_root_view)

return nil

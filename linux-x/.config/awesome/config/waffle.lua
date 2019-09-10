local action = require((...):match("(.-)[^%.]+$") .. "action")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local waffle = require("waffle")
local watch = require("awful.widget.watch")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local icons = require("icons")
local dpi = require("beautiful.xresources").apply_dpi
                      
local waffle_width = beautiful.waffle_width or dpi(200)
local button_height = dpi(24)

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
   local width = waffle_width - dpi(10)
   if args.icon ~= nil then
      width = width - button_height
   end

   local textbox = wibox.widget {
      markup = args.markup or nil,
      text = args.text or nil,
      font = beautiful.fontname_mono .. " 12",
      forced_width = width,
      forced_height = button_height,
      align = "center",
      widget = wibox.widget.textbox,
   }

   local ret

   if args.icon ~= nil then
      ret = wibox.widget {
         {
            {
               {
                  image = args.icon,
                  resize = true,
                  forced_width = button_height,
                  forced_height = button_height,
                  widget = wibox.widget.imagebox,
               },
               textbox,
               layout = wibox.layout.fixed.horizontal
            },
            buttons = action and awful.util.table.join(
                     awful.button({ }, 1, nil, function () action(false) end),
                     awful.button({ }, 3, nil, function () action(true) end)),
            margins = dpi(5),
            widget = wibox.container.margin,
         },
         fg = beautiful.fg_normal,
         bg = beautiful.bg_normal,
         widget = wibox.container.background
      }
   else
      ret = wibox.widget {
         {
            textbox,
            buttons = action and awful.util.table.join(
               awful.button({ }, 1, nil, function () action(false) end),
               awful.button({ }, 3, nil, function () action(true) end)),
            margins = dpi(5),
            widget = wibox.container.margin,
         },
         fg = beautiful.fg_normal,
         bg = beautiful.bg_normal,
         widget = wibox.container.background
      }
   end

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
      ret.keys = {
         [args.key] = function (mod, _, event)
            for _, m in ipairs(mod) do
               mod[m] = true
            end
            if event == "press" then
               action(mod["Shift"])
            end
         end
      }
   end

   ret.textbox = textbox

   return ret
end

local function em(t)
   return "<span color='#a9444e'>" .. t .. "</span>"
end

local function with_background_and_border(widget)
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
   self.textbox.markup = em("P") .. "ower off <span size='x-small'>(" .. tostring(2 - waffle_poweroff_count) .. " more times)</span>"
end

local waffle_poweroff_view = create_view(with_background_and_border(waffle_poweroff_button))
local waffle_settings_view = create_view(
   with_background_and_border(
      wibox.widget {
         simple_button({
               markup = em("S") .. "creen layout",
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
         simple_button({
               markup = "Pulse " .. em("a") .. "udio",
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

   ram_widget = wibox.widget {
      wibox.container.mirror(ramgraph_widget, { horizontal = true }),
      widget = wibox.container.margin
   }

   local prev_usage = 0

   watch({"egrep", "-e", "MemTotal:|MemAvailable:", "/proc/meminfo"}, 1,
      function(widget, stdout, stderr, exitreason, exitcode)
         local total, available = stdout:match('MemTotal:%s+([0-9]+) .*MemAvailable:%s+([0-9]+)')
         local usage = math.floor((total - available) / total * 100 + 0.5)

         widget:add_value(usage - prev_usage)
      end,
      ramgraph_widget
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
         left = dpi(4),
         top = dpi(11),
         bottom = dpi(11),
      },
      widget = wibox.widget.progressbar
   }

   local update_graphic = function(widget, stdout, _, _, _)
      local mute = string.match(stdout, "%[(o%D%D?)%]")
      local volume = string.match(stdout, "(%d?%d?%d)%%")
      volume = tonumber(string.format("% 3d", volume))

      widget.value = volume / 100;
      widget.color = mute == "off" and mute_color
         or bar_color

   end

   volumebar_widget:connect_signal("button::press", function(_,_,_,button)
                                      if (button == 4)     then awful.spawn(INC_VOLUME_CMD, false)
                                      elseif (button == 5) then awful.spawn(DEC_VOLUME_CMD, false)
                                      elseif (button == 1) then awful.spawn(TOG_VOLUME_CMD, false)
                                      end

                                      awful.spawn.easy_async(
                                         GET_VOLUME_CMD, function(stdout, stderr, exitreason, exitcode)
                                            update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
                                      end)
   end)

   watch(GET_VOLUME_CMD, 1, update_graphic, volumebar_widget)
end

local waffle_root_view = create_view(
   wibox.widget {
      with_background_and_border(
         wibox.widget {
            {
               {
                  cpu_widget,
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
                  ram_widget,
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
                  layout = wibox.layout.fixed.horizontal,
               },
               layout = wibox.layout.fixed.vertical,
            },
            bg = beautiful.bg_normal,
            widget = wibox.container.background,
      }),
      with_background_and_border(
         wibox.widget {
            simple_button({
                  icon = gcolor.recolor_image(icons.browser, beautiful.fg_normal),
                  markup = em("W") .. "eb browser",
                  key = "w",
                  action = function (alt)
                     action.web_browser()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.file_manager, beautiful.fg_normal),
                  markup = "Fil" .. em("e") .. " manager",
                  key = "e",
                  action = function (alt)
                     action.file_manager()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.terminal, beautiful.fg_normal),
                  markup = em("T") .. "erminal",
                  key = "t",
                  action = function (alt)
                     action.terminal()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.setup, beautiful.fg_normal),
                  markup = em("S") .. "ettings",
                  key = "s",
                  action = function (alt)
                     waffle:show(waffle_settings_view, true)
                  end
            }),
            layout = wibox.layout.fixed.vertical,
      }),
      with_background_and_border(
         wibox.widget {
            {
               {
                  volumebar_widget,
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
                  layout = wibox.layout.fixed.horizontal,
               },
               layout = wibox.layout.fixed.vertical,
            },
            bg = beautiful.bg_normal,
            widget = wibox.container.background,
      }),
      with_background_and_border(
         wibox.widget {
            simple_button({
                  icon = gcolor.recolor_image(icons.lock, beautiful.fg_normal),
                  markup = em("L") .. "ock screen",
                  key = "l",
                  action = function (alt)
                     action.screen_locker()
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.sleep, beautiful.fg_normal),
                  markup = "S" .. em("u") .. "spend",
                  key = "u",
                  action = function (alt)
                     awful.spawn({"systemctl", "suspend"})
                     waffle:hide()
                  end
            }),
            simple_button({
                  icon = gcolor.recolor_image(icons.poweroff, beautiful.fg_normal),
                  markup = em("P") .. "ower off",
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

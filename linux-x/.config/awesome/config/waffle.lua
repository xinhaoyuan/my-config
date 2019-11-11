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
local gtimer = require("gears.timer")
local icons = require("icons")
local fallback = require("fallback")
local fixed_margin = require("fixed_margin")
local outlined_textbox = require("outlined_textbox")
local cbg = require("contextual_background")
local masked_imagebox = require("masked_imagebox")
local aux = require("aux")
local mpc = require("mpc")
local dpi = require("beautiful.xresources").apply_dpi

local waffle_width = beautiful.waffle_width or dpi(240)
local button_height = dpi(20)
local button_padding = dpi(4)
local font_big = beautiful.fontname_normal .. " 10"
local font_small = beautiful.fontname_mono .. " 7"
local graph_background = "#00000000"
local graph_normal_color = aux.color.from_string(beautiful.border_focus):blend_with(beautiful.bg_normal, 0.25):to_string()
local graph_color = graph_normal_color -- "linear:0,0:0,22:0,#FF0000:0.5," .. graph_normal_color
local update_interval_s = 1

local function em(t)
    -- return "<span color='" .. beautiful.special_normal .. "'>" .. t .. "</span>"
    return t
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

local function context_focused(context)
    context.focus = true
end

local function simple_button(args)
   local action = args.action
   local width = waffle_width - button_padding * 2

   local label = args.label_widget or
      wibox.widget {
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
             args.icon_widget or (
                 args.icon and
                     wibox.widget {
                         image = args.icon,
                         resize = true,
                         forced_width = button_height,
                         forced_height = button_height,
                         widget = masked_imagebox,
                     }
                                 ),
            label,
            args.indicator and {
                {
                    text = args.indicator,
                    font = font_big,
                    forced_height = button_height,
                    align = "center",
                    valign = "center",
                    widget = wibox.widget.textbox,
                },
                fg_function = {"special_"},
                widget = cbg
            },
            forced_width = width,
            layout = wibox.layout.align.horizontal,
         },
         buttons = args.buttons or
            (action and
                awful.util.table.join(
                   awful.button({ }, 1, function () action(false) end),
                   awful.button({ }, 3, function () action(true) end))),
         margins = button_padding,
         widget = wibox.container.margin,
      },
      fg_function = {"fg_"},
      bg_function = {"bg_"},
      widget = cbg
   }

   ret:connect_signal(
      "mouse::enter",
      function ()
          ret:set_context_transform_function({focus = true})
      end
   )

   ret:connect_signal(
      "mouse::leave",
      function ()
          ret:set_context_transform_function(nil)
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

   ret.label = label

   return ret
end

local function decorate(widget)
   return wibox.widget {
      {
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
         widget = wibox.container.margin,
      },
      bg = beautiful.border_focus,
      widget = wibox.container.background,
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
   self.label.markup = "Power off <span size='x-small'>(" .. tostring(2 - waffle_poweroff_count) .. " more times)</span>"
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
                      shared.var.hide_clients_with_titlebars =
                          not shared.var.hide_clients_with_titlebars
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
                  beautiful.bar_style = "auto"
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

local cpu_widget_width = (waffle_width - button_padding) / 2 - button_height - button_padding * 2
local cpu_widget
do
   local cpugraph_widget = wibox.widget {
      max_value = 100,
      background_color = graph_background,
      forced_width = cpu_widget_width,
      forced_height = button_height / 2,
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      color = graph_color
   }

   local cpu_text_widget = wibox.widget {
       forced_width = cpu_widget_width,
       forced_height = button_height,
       align = "center",
       point = {x = 0, y = 0},
       outline_color = beautiful.bg_normal,
       outline_size = dpi(2),
       widget = outlined_textbox,
   }

   cpu_widget = wibox.widget {
      {
          {
              cpugraph_widget,
              {
                  cpugraph_widget,
                  reflection = {
                      horizontal = false,
                      vertical = true,
                  },
                  widget = wibox.container.mirror,
              },
              layout = wibox.layout.fixed.vertical,
          },
         cpu_text_widget,
         layout = wibox.layout.manual
      },
      width = cpu_widget_width,
      height = button_height,
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

local ram_widget_width = (waffle_width - button_padding) / 2 - button_height - button_padding * 2
local ram_widget
do
   local ramgraph_widget = wibox.widget {
      max_value = 100,
      background_color = graph_background,
      forced_width = ram_widget_width,
      forced_height = button_height / 2,
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
      color = graph_color
   }

   local ram_text_widget = wibox.widget {
       forced_width = ram_widget_width,
       forced_height = button_height,
       align = "center",
       point = {x = 0, y = 0},
       outline_color = beautiful.bg_normal,
       outline_size = dpi(2),
       widget = outlined_textbox,
   }

   ram_widget = wibox.widget {
       {
           {
               ramgraph_widget,
               {
                   ramgraph_widget,
                   reflection = {
                       horizontal = false,
                       vertical = true,
                   },
                   widget = wibox.container.mirror,
               },
               layout = wibox.layout.fixed.vertical,
           },
           ram_text_widget,
           layout = wibox.layout.manual
       },
       width = ram_widget_width,
       height = button_height,
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

local net_widget_width = (waffle_width - button_height - button_padding * 4) / 2
local net_widget
do
    local netgraph_rx_widget = wibox.widget {
        background_color = graph_background,
        forced_width = net_widget_width,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local rx_text_widget = wibox.widget {
        forced_width = net_widget_width,
        forced_height = button_height,
        align = "center",
        point = {x = 0, y = 0},
        outline_color = beautiful.bg_normal,
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local net_rx_widget = wibox.widget {
        netgraph_rx_widget,
        point = {x = 0, y = 0},
        widget = wibox.container.margin
    }

    local net_rx_layout = wibox.widget {
        {
            {
                net_rx_widget,
                {
                    net_rx_widget,
                    reflection = {
                        horizontal = false,
                        vertical = true,
                    },
                    widget = wibox.container.mirror,
                },
                layout = wibox.layout.fixed.vertical,
            },
            rx_text_widget,
            layout = wibox.layout.manual,
        },
        width = net_widget_width,
        height = button_height,
        widget = wibox.container.constraint,
    }

    local netgraph_tx_widget = wibox.widget {
        background_color = graph_background,
        forced_width = net_widget_width,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local tx_text_widget = wibox.widget {
        forced_width = net_widget_width,
        forced_height = button_height,
        align = "center",
        point = { x = 0, y = 0 },
        outline_color = beautiful.bg_normal,
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local net_tx_widget = wibox.widget {
        netgraph_tx_widget,
        widget = wibox.container.margin
    }

    local net_tx_layout = wibox.widget {
        {
            {
                net_tx_widget,
                {
                    net_tx_widget,
                    reflection = {
                        horizontal = false,
                        vertical = true,
                    },
                    widget = wibox.container.mirror,
                },
                layout = wibox.layout.fixed.vertical,
            },
            tx_text_widget,
            layout = wibox.layout.manual,
        },
        width = net_widget_width,
        height = button_height,
        widget = wibox.container.constraint,
    }

    net_widget = wibox.widget {
        {
            image = gcolor.recolor_image(icons.ethernet, beautiful.fg_normal),
            forced_height = button_height,
            forced_width = button_height,
            widget = wibox.widget.imagebox,
        },
        net_rx_layout,
        net_tx_layout,
        spacing = button_padding,
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
                if items[1] ~= "lo" and items[1]:match("^tun[0-9]*$") == nil then
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

local volumebar_widget_width = waffle_width - button_height
local volumebar_widget
do
   local GET_VOLUME_CMD = 'amixer -D pulse sget Master'
   local INC_VOLUME_CMD = 'amixer -D pulse sset Master 5%+'
   local DEC_VOLUME_CMD = 'amixer -D pulse sset Master 5%-'
   local TOG_VOLUME_CMD = 'amixer -D pulse sset Master toggle'

   local bar_color = graph_normal_color
   local mute_color = beautiful.special_normal
   local background_color = beautiful.border_normal

   volumebar_widget = wibox.widget {
      max_value = 1,
      forced_width = volumebar_widget_width,
      forced_height = button_height,
      paddings = 0,
      border_width = 0,
      color = bar_color,
      background_color = background_color,
      shape = gshape.bar,
      clip = true,
      margins = {
         left = dpi(4) + button_padding,
         right = dpi(4),
         top = button_height / 2 - dpi(1),
         bottom = button_height / 2 - dpi(1),
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
         if (button == 1)     then
             awful.spawn({"pavucontrol"})
             waffle:hide()
         elseif (button == 3) then cmd = TOG_VOLUME_CMD
         elseif (button == 4) then cmd = INC_VOLUME_CMD
         elseif (button == 5) then cmd = DEC_VOLUME_CMD
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

local mpd_widget
do
    -- Not used anymore
    -- local GET_STATUS_CMD = 'mpc current -f "%title% - %artist%" -q'
    -- local NEXT_CMD = 'mpc next -q'
    -- local PREV_CMD = 'mpc prev -q'
    -- local TOGGLE_CMD = 'mpc toggle -q'

    local mpd_status_widget = wibox.widget {
        text = "",
        forced_height = button_height,
        font = beautiful.fontname_normal.." 14",
        -- outline_color = beautiful.bg_normal,
        -- outline_size = dpi(2),
        -- widget = outlined_textbox
        widget = wibox.widget.textbox
    }

    local mpd_text_widget = wibox.widget {
        text = "",
        align = "center",
        valign = "center",
        ellipsize = "end",
        forced_height = button_height,
        widget = wibox.widget.textbox
    }

    local lighter_fg_normal = aux.color.from_string(beautiful.fg_normal):blend_with(beautiful.bg_normal, 0.75):to_string()
    local lighter_fg_focus = aux.color.from_string(beautiful.fg_focus):blend_with(beautiful.bg_focus, 0.75):to_string()

    local mpd_icon_widget =
        wibox.widget {
            {
                image = gcolor.recolor_image(icons.music, beautiful.fg_normal),
                resize = true,
                forced_width = button_height,
                forced_height = button_height,
                widget = masked_imagebox,
            },
            fg_function = function (context)
                if context.focus then
                    return lighter_fg_focus
                else
                    return lighter_fg_normal
                end
            end,
            widget = cbg
        }

    local mpd_need_update = false
    local mpc_conn
    local mpc_ping_timer = gtimer {
        timeout = 3,
        callback = function ()
            mpc_conn:send("ping")
        end
    }
    local function mpc_error_handler(err)
        mpd_status_widget:set_text("✖")
        mpd_text_widget:set_text(tostring(err))
    end

    mpc_conn = mpc.new(
        nil, nil, nil, mpc_error_handler,
        "status",
        function(_, result)
            if result.state == "play" then
                mpd_status_widget:set_text("▶")
            elseif result.state == "pause" then
                mpd_status_widget:set_text("⏸")
            elseif result.state == "stop" then
                mpd_status_widget:set_text("⏹")
            else
                mpd_status_widget:set_text(result.state)
            end
        end,
        "currentsong",
        function(_, result)
            local title, artist, file = result.title, result.artist, result.file
            -- for k, v in pairs(result) do
            --     print(tostring(k)..":"..tostring(v))
            -- end
            local text = ""
            if title then
                text = title
            end
            if artist then
                text = text.." - "..artist
            end
            mpd_text_widget:set_text(text)
        end
    )
    mpc_ping_timer:start()

    mpd_widget = simple_button {
        icon_widget = wibox.widget {
            mpd_icon_widget,
            {
                mpd_status_widget,
                widget = wibox.container.place
            },
            layout = wibox.layout.stack
        },
        label_widget = wibox.widget {
            fixed_margin(
                wibox.widget {
                    mpd_text_widget,
                    draw_empty = false,
                    left = dpi(5),
                    right = dpi(5),
                    widget = wibox.container.margin,
                }
            ),
            {
                text = "Music",
                align = "center",
                valign = "center",
                forced_height = button_height,
                widget = wibox.widget.textbox,
            },
            widget = fallback,
        },
        indicator = em("m"),
        key = "m",
        action = function (alt)
            waffle:hide()
            shared.action.music_app()
        end,
        buttons = awful.util.table.join(
            awful.button({ }, 1, function () waffle:hide(); shared.action.music_app() end),
            awful.button({ }, 3, function () mpc_conn:toggle_play() end),
            -- Avoid accidental multi prev/next actions
            awful.button({ }, 4, function ()
                    mpc_conn:send("next")
            end),
            awful.button({ }, 5, function ()
                    mpc_conn:send("previous")
            end)
        ),
    }

    mpd_widget.keys["Left"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_conn:send("previous")
        end
    end
    mpd_widget.keys["Right"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_conn:send("next")
        end
    end
    mpd_widget.keys["Up"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_conn:toggle_play()
        end
    end
end

local waffle_root_view = create_view(
   wibox.widget {
      --    wibox.widget {
      --       align = "center",
      --       format = "<span size='x-large'>%y-%m-%d %a %H:%M</span>",
      --       widget = wibox.widget.textclock,
      --    },
      decorate(
         wibox.widget {
            {
               {
                  {
                     image = gcolor.recolor_image(icons.cpu, beautiful.fg_normal),
                     forced_height = button_height,
                     forced_width = button_height,
                     widget = wibox.widget.imagebox,
                  },
                  cpu_widget,
                  {
                     image = gcolor.recolor_image(icons.ram, beautiful.fg_normal),
                     forced_height = button_height,
                     forced_width = button_height,
                     widget = wibox.widget.imagebox,
                  },
                  ram_widget,
                  spacing = button_padding,
                  layout = wibox.layout.fixed.horizontal,
               },
               net_widget,
               spacing = button_padding,
               layout = wibox.layout.fixed.vertical,
            },
            margins = button_padding,
            widget = wibox.container.margin,
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
               mpd_widget,
               {
                  text = "Music",
                  widget = wibox.widget.textbox,
               },
               widget = fallback,
            },
            {
               {
                  {
                     image = gcolor.recolor_image(icons.audio, beautiful.fg_normal),
                     forced_height = button_height,
                     forced_width = button_height,
                     widget = wibox.widget.imagebox,
                  },
                  volumebar_widget,
                  forced_width = waffle_width - button_padding * 2,
                  layout = wibox.layout.fixed.horizontal,
               },
               margins = button_padding,
               widget = wibox.container.margin,
            },
            layout = wibox.layout.fixed.vertical,
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

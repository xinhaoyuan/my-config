local shared = require((...):match("(.-)[^%.]+$") .. "shared")
shared.waffle = {}

local capi = {
   client = client,
   screen = screen,
}

local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local waffle = require("waffle")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local gtimer = require("gears.timer")
local gstring = require("gears.string")
local icons = require("icons")
local fallback = require("fallback")
local fixed_margin = require("fixed_margin")
local fixed_align = require("fixed_align")
local outlined_textbox = require("outlined_textbox")
local cbg = require("contextual_background")
local debug_container = require("debug_container")
local masked_imagebox = require("masked_imagebox")
local border = require("border-theme")
local acolor = require("aux").color
local mpc_gobject = require("mpc-gobject")
local orgenda = require("orgenda")
local dpi = require("beautiful.xresources").apply_dpi

local panel_border = true
local panel_outer = true
local waffle_width = beautiful.waffle_panel_width or dpi(240)
local button_height = beautiful.waffle_item_height or dpi(20)
local button_padding = dpi(4)
local panel_padding = dpi(8)
local font_normal = beautiful.fontname_normal.." "..tostring(beautiful.fontsize_normal)
local font_info = beautiful.fontname_normal.." "..tostring(beautiful.fontsize_small)
local graph_background = "#00000000"
local graph_normal_color = acolor.from_string(beautiful.bg_focus):blend_with(beautiful.bg_normal, 0.25):to_string()
local graph_color = graph_normal_color -- "linear:0,0:0,22:0,#FF0000:0.5," .. graph_normal_color
local update_interval_s = 2

local function em(t)
    -- return "<span color='" .. beautiful.special_normal .. "'>" .. t .. "</span>"
    return t
end

local function view(args)
    args = args or {}
    local root = args.root
    assert(root ~= nil)
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
        elseif args.default_key_handler then
            return args.default_key_handler(mod, key, event)
        else
            return false
        end
    end

    view.options = {
        -- Options here.
    }

    view.on_close = args.on_close

    return view
end

local function context_focused(context)
    context.focus = true
end

local function simple_button(args)
   local button_action = args.button_action or args.action
   local key_action = args.key_action or args.action

   local ret = wibox.widget {
      {
         args.widget,
         buttons = args.buttons or
            (button_action and
                awful.util.table.join(
                   awful.button({ }, 1, function () button_action(false) end),
                   awful.button({ }, 3, function () button_action(true) end))),
         margins = button_padding,
         widget = wibox.container.margin,
      },
      forced_width = args.width,
      forced_height = args.height,
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

   if args.key ~= nil and key_action then
      local function cb(mod, _, event)
         for _, m in ipairs(mod) do
            mod[m] = true
         end
         if event == "press" then
            key_action(mod["Shift"])
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

   return ret
end


local function button(args)
   local label = args.label_widget or
      wibox.widget {
         markup = args.markup,
         text = args.text,
         font = font_normal,
         forced_height = button_height,
         align = "center",
         valign = "center",
         widget = wibox.widget.textbox,
      }

   args.width = args.width or waffle_width

   args.widget = wibox.widget {
       args.icon_widget or (
           args.icon and
               wibox.widget {
                   image = args.icon,
                   resize = true,
                   forced_width = args.icon_width or button_height,
                   forced_height = args.icon_height or button_height,
                   widget = masked_imagebox,
               }
                           ),
       label,
       args.indicator and {
               {
                   text = args.indicator,
                   font = font_normal,
                   align = "center",
                   valign = "center",
                   widget = wibox.widget.textbox,
               },
           fg_function = {"special_"},
           widget = cbg
                          },
       expand = "inside",
       layout = args.button_layout or fixed_align.horizontal,
   }

   local ret = simple_button(args)
   ret.label = label

   return ret
end

local border_theme
if beautiful.border_radius == nil then
    border_theme = border.default_theme
else
    border_theme = setmetatable({}, {__index = border.rounded_theme})
    border_theme:init()
    border_theme.size = beautiful.border_radius
    border_theme.outer_space = beautiful.border_outer_space
    border_theme.inner_space = beautiful.border_radius - beautiful.border_width + beautiful.border_inner_space
end

local decorate_border = border.directions { "top", "bottom", "left", "right" }
local function decorate_panel(widget)
    local panel = wibox.widget {
        widget,
        bg = beautiful.bg_normal,
        fg = beautiful.fg_normal,
        shape = beautiful.border_radius ~= nil and
            function (cr, width, height)
                gshape.rounded_rect(cr, width, height,
                                    beautiful.border_radius -
                                        beautiful.border_width)
            end,
        widget = wibox.container.background
    }

    if panel_border then
        panel = wibox.widget {
            {
                panel,
                margins = beautiful.border_width,
                draw_empty = false,
                widget = fixed_margin,
            },
            bgimage = function (context, cr, width, height)
                border:draw({ theme = border_theme, color = beautiful.border_focus }, cr, width, height, decorate_border)
            end,
            shape = beautiful.border_radius ~= nil and function (cr, width, height)
                gshape.rounded_rect(cr, width, height, beautiful.border_radius)
                                                       end,
            widget = wibox.container.background
        }
    end

    return panel
end

local function decorate_waffle(widget)
    if panel_outer then
        return wibox.widget {
            {
                widget,
                margins = panel_padding,
                widget = wibox.container.margin,
            },
            bg = acolor.from_string(beautiful.fg_normal.."a0"):blend_with(beautiful.bg_normal.."a0", 0.75):to_string(),
            shape = beautiful.border_radius ~= nil and
            function (cr, width, height)
                gshape.rounded_rect(cr, width, height,
                                    beautiful.border_radius + panel_padding)
            end,
            widget = wibox.container.background,
        }
    else
        return wibox.widget.base.make_widget_from_value(widget)
    end
end


local hide_key_pressed = false
local function hide_after_release(mod, key, event)
    if event == "press" and not hide_after_pressed then
        hide_after_pressed = true
    elseif event == "release" and hide_after_pressed then
        gtimer.start_new(
            0.03, -- Fixing xscape emulating the key case the waffle to show again.
            function ()
                hide_after_pressed = false
                waffle:hide()
            end
        )
    end
end

local waffle_shutdown_view = view {
    root = decorate_waffle(decorate_panel {
        button {
                -- icon = gcolor.recolor_image(icons.sleep, beautiful.fg_normal),
                markup = "Suspend",
                indicator = em("s"),
                key = "s",
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "suspend"})
                end
        },
        button {
                -- icon = gcolor.recolor_image(icons.sleep, beautiful.fg_normal),
                markup = "Hibernate",
                indicator = em("h"),
                key = "h",
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "hibernate"})
                end
        },
        button {
                markup = "Reboot",
                indicator = em("r"),
                key = "r",
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "reboot"})
                end
        },
        button {
                -- icon = gcolor.recolor_image(icons.poweroff, beautiful.fg_normal),
                markup = "Power off",
                indicator = em("p"),
                key = "p",
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "poweroff"})
                end
        },
        layout = wibox.layout.fixed.vertical
    })
}

local waffle_settings_view

local cpu_widget_width = (waffle_width - button_padding) / 2 - button_height - button_padding * 2
local cpu_widget
do
   local cpu_graph_widget = wibox.widget {
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
              {
                  id = "graph",
                  widget = cpu_graph_widget
              },
              {
                  cpu_graph_widget,
                  reflection = {
                      horizontal = false,
                      vertical = true,
                  },
                  widget = wibox.container.mirror,
              },
              layout = wibox.layout.fixed.vertical,
          },
          {
              id = "text",
              widget = cpu_text_widget
          },
         layout = wibox.layout.manual
      },
      height = button_height,
      widget = wibox.container.constraint,
   }

   local total_prev = 0
   local idle_prev = 0
   local function on_output (stdout)
       local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
           stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')

       local total = user + nice + system + idle + iowait + irq + softirq + steal

       local diff_idle = idle - idle_prev
       local diff_total = total - total_prev
       local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10

       local markup = "<span font_desc='" .. font_info .. "'>" .. tostring(math.floor(diff_usage)) .. "%</span>"
       -- widget:get_children_by_id("text")[1]:set_markup(markup)
       -- widget:get_children_by_id("graph")[1]:add_value(diff_usage)
       cpu_text_widget:set_markup(markup)
       cpu_graph_widget:add_value(diff_usage)

       total_prev = total
       idle_prev = idle
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async({"grep", "-e", "^cpu ", "/proc/stat"}, on_output)
       end,
   }
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
   local ram_graph_widget = wibox.widget {
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
               {
                   id = "graph",
                   widget = ram_graph_widget
               },
               {
                   ram_graph_widget,
                   reflection = {
                       horizontal = false,
                       vertical = true,
                   },
                   widget = wibox.container.mirror,
               },
               layout = wibox.layout.fixed.vertical,
           },
           {
               id = "text",
               widget = ram_text_widget
           },
           layout = wibox.layout.manual
       },
       height = button_height,
       widget = wibox.container.constraint,
   }

   local function on_output(stdout)
       local total, available = stdout:match('MemTotal:%s+([0-9]+) .*MemAvailable:%s+([0-9]+)')
       local usage = math.floor((total - available) / total * 100 + 0.5)

       local markup = "<span font_desc='" .. font_info .. "'>" .. format_size((total - available) * 1000) .. "B</span>"
       -- widget:get_children_by_id("text")[1]:set_markup(markup)
       -- widget:get_children_by_id("graph")[1]:add_value(usage)
       ram_text_widget:set_markup(markup)
       ram_graph_widget:add_value(usage)
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async({"egrep", "-e", "MemTotal:|MemAvailable:", "/proc/meminfo"}, on_output)
       end,
   }
end

local net_widget_width = (waffle_width - button_height - button_padding * 4) / 2
local net_widget
do
    local netgraph_rx_widget = wibox.widget {
        background_color = graph_background,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local rx_text_widget = wibox.widget {
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
            layout = wibox.layout.stack,
        },
        height = button_height,
        widget = wibox.container.constraint,
    }

    local netgraph_tx_widget = wibox.widget {
        background_color = graph_background,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local tx_text_widget = wibox.widget {
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
            layout = wibox.layout.stack,
        },
        height = button_height,
        widget = wibox.container.constraint,
    }

    net_widget = wibox.widget {
        {
            image = gcolor.recolor_image(icons.ethernet, beautiful.fg_normal),
            forced_height = button_height,
            forced_width = button_height,
            widget = masked_imagebox,
        },
        {
            {
                net_rx_layout,
                net_tx_layout,
                spacing = button_padding,
                layout = wibox.layout.flex.horizontal
            },
            left = button_padding,
            right = button_padding,
            layout = wibox.container.margin
        },
        expand = "inside",
        layout = wibox.layout.align.horizontal,
    }

    local prev_recv = nil
    local prev_send = nil
    local function on_output(stdout)
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
                -- Skips VPN for avoiding double-counting
                recv = recv + tonumber(items[2])
                send = send + tonumber(items[10])
            end
        end

        if prev_recv ~= nil then
            local rx = (recv - prev_recv) / update_interval_s
            local markup = "<span font_desc='" .. font_info .. "'>R " .. format_size(rx) .. "B/s</span>"
            rx_text_widget:set_markup(markup)
            netgraph_rx_widget.max_value = 256 * 1024
            netgraph_rx_widget:add_value(rx)
        end
        prev_recv = recv
        if prev_send ~= nil then
            local tx = (send - prev_send) / update_interval_s
            local markup = "<span font_desc='" .. font_info .. "'>T " .. format_size(tx) .. "B/s</span>"
            tx_text_widget:set_markup(markup)
            netgraph_tx_widget.max_value = 256 * 1024
            netgraph_tx_widget:add_value(tx)
        end
        prev_send = send
    end

    gtimer {
        timeout = update_interval_s,
        call_now = true,
        autostart = true,
        callback = function ()
            awful.spawn.easy_async({"egrep", "-e", "[[:alnum:]]+:", "/proc/net/dev"}, on_output)
        end,
    }
end

local battery_widget_width = waffle_width - button_height - button_padding * 3
local battery_widget
do
   local bar_color = graph_normal_color
   local charging_color = acolor.from_string(beautiful.special_normal):blend_with(beautiful.bg_normal, 0.25):to_string()
   local background_color = beautiful.border_normal

   local battery_status_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        align = "center",
        forced_width = battery_widget_width,
        forced_height = button_height - dpi(2),
        font = font_info,
        widget = wibox.widget.textbox
    }

   local battery_percentage_widget = wibox.widget {
      max_value = 1,
      forced_width = battery_widget_width,
      forced_height = dpi(2),
      paddings = 0,
      border_width = 0,
      color = bar_color,
      background_color = background_color,
      shape = gshape.bar,
      clip = true,
      widget = wibox.widget.progressbar
   }

   battery_widget = wibox.widget {
       {
           image = gcolor.recolor_image(icons.battery_full, beautiful.fg_normal),
           forced_height = button_height,
           forced_width = button_height,
           widget = wibox.widget.imagebox
       },
       {
           battery_status_widget,
           battery_percentage_widget,
           layout = wibox.layout.fixed.vertical
       },
       spacing = button_padding,
       visible = false,
       layout = wibox.layout.fixed.horizontal
   }

   -- Surface-linux
   local battery_status_command = {"mshw0084-rqst.py", "-q", "-d", "/dev/ttyS0", "bat1.pretty"}
   -- -- For debugging
   -- local battery_status_command = {"echo", "Percentage: 70%\nState:Charging\nRemaining: 10 hours"}

   local function parse_battery_output(stdout)
       local results = {}
       for line in stdout:gmatch("[^\r\n]+") do
           key, value = line:match("%s*([^:]-)%s*:%s*(.-)%s*$")
           if key == "Percentage" then
               results.value = tonumber(value:match("(%d*)%%")) / 100
           elseif key == "Remaining" then
               results.remaining = value
           elseif key == "State" then
               results.state = value
               results.charging = (value:match("Charging") ~= nil)
           end
       end
       return results
   end

   local update_graphic = function (widget, stdout)
       local status = parse_battery_output(stdout)
       battery_widget.visible = true
       battery_percentage_widget.value = status.value
       battery_percentage_widget.color = status.charging and charging_color or bar_color
       battery_status_widget:set_text(status.state..": "..status.remaining)
   end

   local function spawn_and_update_battery(cmd)
       awful.spawn.easy_async_with_shell(
           battery_status_command,
           function (stdout, stderr, exitreason, exitcode)
               update_graphic(battery_widget, stdout, stderr, exitreason, exitcode)
           end
       )
   end

   gtimer {
       timeout = 60,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async(battery_status_command,
                                  function (stdout)
                                      update_graphic(battery_widget, stdout)
                                  end
           )
       end,
   }
end


local volumebar_widget_width = waffle_width - button_height
local volumebar_widget
local volumebar_buttons
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
      forced_width = waffle_width,
      forced_height = dpi(2),
      paddings = 0,
      border_width = 0,
      color = bar_color,
      background_color = background_color,
      shape = gshape.bar,
      clip = true,
      margins = {
         left = button_padding,
         right = button_padding,
      },
      widget = wibox.widget.progressbar
   }

   local update_graphic = function (widget, stdout)
      local mute = string.match(stdout, "%[(o%D%D?)%]")
      local volume = string.match(stdout, "(%d?%d?%d)%%")
      volume = tonumber(string.format("% 3d", volume))

      widget.value = volume / 100;
      widget.color = mute == "off" and mute_color
         or bar_color

   end

   local function spawn_and_update_volumebar(cmd)
       awful.spawn.easy_async_with_shell(
           cmd .. ">/dev/null&&" .. GET_VOLUME_CMD,
           function (stdout, stderr, exitreason, exitcode)
               update_graphic(volumebar_widget, stdout, stderr, exitreason, exitcode)
           end
       )
   end

   volumebar_buttons = awful.util.table.join(
       awful.button({ }, 1, function ()
               awful.spawn({"pavucontrol"})
               waffle:hide()
       end),
       awful.button({ }, 3, function ()
               spawn_and_update_volumebar(TOG_VOLUME_CMD)
       end),
       awful.button({ }, 4, function ()
               spawn_and_update_volumebar(INC_VOLUME_CMD)
       end),
       awful.button({ }, 5, function ()
               spawn_and_update_volumebar(DEC_VOLUME_CMD)
       end)
   )

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async(GET_VOLUME_CMD,
                                  function (stdout)
                                      update_graphic(volumebar_widget, stdout)
                                  end
           )
       end,
   }

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

    local mpd_title_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        forced_height = button_height,
        widget = wibox.widget.textbox
    }

    local mpd_meta_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        font = beautiful.fontname_normal.." "..tostring(beautiful.fontsize_small),
        forced_height = button_height,
        widget = wibox.widget.textbox
    }

    local mpd_progress_widget = wibox.widget {
        max_value     = 1,
        value         = 0,
        forced_height = dpi(2),
        color = graph_normal_color,
        background_color = beautiful.border_normal,
        widget        = wibox.widget.progressbar,
    }

    -- local lighter_fg_normal = acolor.from_string(beautiful.fg_normal):blend_with(beautiful.bg_normal, 0.75):to_string()
    -- local lighter_fg_focus = acolor.from_string(beautiful.fg_focus):blend_with(beautiful.bg_focus, 0.75):to_string()

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
                    return beautiful.fg_focus
                else
                    return beautiful.fg_normal
                end
            end,
            widget = cbg
        }

    mpc_gobject:connect_signal(
        "update::status",
        function (_, status)
            if status.err then
                -- mpd_status_widget:set_text("✖")
                -- mpd_title_widget:set_text(tostring(err))
                -- mpd_meta_widget:set_text("(╯°Д°)╯ ┻━┻")
                mpd_widget:set_visible(false)
                return
            end

            if status.state == "play" then
                mpd_status_widget:set_text("▶")
            elseif status.state == "pause" then
                mpd_status_widget:set_text("⏸")
            elseif status.state == "stop" then
                mpd_status_widget:set_text("⏹")
            else
                mpd_status_widget:set_text(status.state)
            end

            mpd_progress_widget:set_value(status.progress or 0)
            mpd_widget:set_visible(true)
        end
    )

    mpc_gobject:connect_signal(
        "update::song",
        function (_, song)
            mpd_title_widget:set_text(song.title or "")

            local meta = {}
            if song.artist then
                meta[#meta + 1] = song.artist
            end
            if song.album then
                meta[#meta + 1] = song.album
            end

            mpd_meta_widget:set_text(table.concat(meta, " - "))
        end
    )

    mpd_widget = button {
        icon_widget = wibox.widget {
            {
                mpd_status_widget,
                widget = wibox.container.place
            },
            {
                mpd_icon_widget,
                widget = wibox.container.place
            },
            layout = wibox.layout.fixed.vertical,
        },
        label_widget = wibox.widget {
            {
                {
                    {
                        {
                            mpd_title_widget,
                            speed = 100,
                            step_function = wibox.container.scroll.step_functions
                                .waiting_nonlinear_back_and_forth,
                            layout = wibox.container.scroll.horizontal,
                        },
                        widget = wibox.container.place
                    },
                    mpd_progress_widget,
                    {
                        {
                            {
                                mpd_meta_widget,
                                speed = 100,
                                step_function = wibox.container.scroll.step_functions
                                    .waiting_nonlinear_back_and_forth,
                                layout = wibox.container.scroll.horizontal,
                            },
                            widget = wibox.container.place
                        },
                        {
                            text = "_(:3」∠)_",
                            align = "center",
                            force_height = button_height - dpi(2),
                            font = font_info,
                            widget = wibox.widget.textbox
                        },
                        widget = fallback
                    },
                    layout = wibox.layout.fixed.vertical,
                },
                draw_empty = false,
                left = button_padding,
                right = button_padding,
                widget = fixed_margin,
            },
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
            awful.button({ }, 3, function () mpc_gobject:toggle_play() end),
            awful.button({ }, 4, function ()
                    mpc_gobject:go_next()
            end),
            awful.button({ }, 5, function ()
                    mpc_gobject:go_previous()
            end)
        ),
    }

    mpd_widget.keys["Left"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:go_previous()
        end
    end
    mpd_widget.keys["Right"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:go_next()
        end
    end
    mpd_widget.keys["Up"] = function (mod)
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:toggle_play()
        end
    end
end

local orgenda_widget
do
    local orgenda_lines = {}
    local orgenda_text = wibox.widget {
        font = font_info,
        ellipsize = "end",
        align = "left",
        wrap = "word_char",
        widget = wibox.widget.textbox
    }
    orgenda_widget = wibox.widget {
        {
            orgenda_text,
            margins = button_padding,
            draw_empty = false,
            widget = fixed_margin,
        },
        width = waffle_width,
        widget = wibox.container.constraint
    }

    local function render_priority(pri)
        return ({ "+", "*", "#" })[pri]
    end

    local function render_orgenda_items(items)
        local lines = {}
        for _, item in ipairs(items) do
            if item.date ~= nil then
                table.insert(lines, "<b>["..tostring(item.date).."]</b>"..render_priority(item.priority).." "..gstring.xml_escape(item.text))
            else
                table.insert(lines, render_priority(item.priority).." "..gstring.xml_escape(item.text))
            end
        end
        return lines
    end

    orgenda.topic:connect_signal(
        "update",
        function (_, path, items)
            orgenda_lines[path] = render_orgenda_items(items)
            local lines = {}
            for k, v in pairs(orgenda_lines) do
                for _, l in ipairs(v) do
                    table.insert(lines, l)
                end
            end
            orgenda_text.markup = table.concat(lines, "\n")
        end
    )
end

local waffle_root_status_widget = decorate_panel {
    {
        button {
            label_widget = {
                {
                    {
                        image = gcolor.recolor_image(icons.cpu, beautiful.fg_normal),
                        forced_height = button_height,
                        forced_width = button_height,
                        widget = masked_imagebox,
                    },
                    {
                        cpu_widget,
                        left = button_padding,
                        right = button_padding,
                        widget = wibox.container.margin
                    },
                    layout = wibox.layout.align.horizontal
                },
                {
                    {
                        image = gcolor.recolor_image(icons.ram, beautiful.fg_normal),
                        forced_height = button_height,
                        forced_width = button_height,
                        widget = masked_imagebox,
                    },
                    {
                        ram_widget,
                        left = button_padding,
                        right = button_padding,
                        widget = wibox.container.margin
                    },
                    expand = "inside",
                    layout = wibox.layout.align.horizontal
                },
                spacing = button_padding,
                layout = wibox.layout.flex.horizontal,
            },
            key = "x",
            indicator = em("x"),
            action = function (alt)
                shared.action.terminal({"htop"})
                waffle:hide()
            end,
        },
        button {
            label_widget = net_widget,
            indicator = em("n"),
        },
        {
            button {
                icon = gcolor.recolor_image(icons.audio, beautiful.fg_normal),
                label_widget = wibox.widget {
                    volumebar_widget,
                    widget = wibox.container.place
                },
                buttons = volumebar_buttons,
                indicator = em("a"),
                key = "a",
                action = function (alt)
                    local cmd = {"pavucontrol"}
                    awful.spawn(cmd)
                    waffle:hide()
                end,
            },
            layout = wibox.layout.fixed.horizontal,
        },
        {
            battery_widget,
            margins = button_padding,
            draw_empty = false,
            widget = fixed_margin,
        },
        layout = wibox.layout.fixed.vertical,
    },
    widget = wibox.container.margin,
}

local waffle_root_agenda_widget = decorate_panel {
    button {
        icon = gcolor.recolor_image(icons.clock, beautiful.fg_normal),
        label_widget = wibox.widget {
            align = "center",
            format = "%m/%d %a %H:%M",
            widget = wibox.widget.textclock,
        },
        indicator = em("t"),
        key = "t",
        action = function (alt)
            if alt then
                shared.action.web_browser("https://calendar.google.com")
            else
                shared.action.calendar()
            end
            waffle:hide()
        end,
    },
    orgenda_widget,
    layout = wibox.layout.fixed.vertical
}

local waffle_root_action_widget = decorate_panel {
    button {
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
    },
    button({
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
    button({
            icon = gcolor.recolor_image(icons.browser, beautiful.fg_normal),
            markup = "Web browser",
            indicator = em("w"),
            key = "w",
            action = function (alt)
                shared.action.web_browser()
                waffle:hide()
            end
    }),
    button({
            icon = gcolor.recolor_image(icons.file_manager, beautiful.fg_normal),
            markup = "File manager",
            indicator = em("e"),
            key = "e",
            action = function (alt)
                shared.action.file_manager()
                waffle:hide()
            end
    }),
    button({
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
    button({
            icon = gcolor.recolor_image(icons.setup, beautiful.fg_normal),
            markup = "Settings",
            indicator = em("s"),
            key = "s",
            action = function (alt)
                waffle:show(waffle_settings_view, { push = true })
            end,
    }),
    layout = wibox.layout.fixed.vertical,
}

local waffle_root_mpd_widget = decorate_panel {
    mpd_widget,
    draw_empty = false,
    layout = fixed_margin,
}

local waffle_root_admin_widget = decorate_panel {
    button {
            icon = gcolor.recolor_image(icons.lock, beautiful.fg_normal),
            markup = "Lock screen",
            indicator = em("l"),
            key = "l",
            action = function (alt)
                shared.action.screen_locker()
                waffle:hide()
            end
    },
    button {
            icon = gcolor.recolor_image(icons.poweroff, beautiful.fg_normal),
            markup = "Shut down",
            indicator = em("u"),
            key = "u",
            action = function (alt)
                waffle:show(waffle_shutdown_view, { push = true })
            end
    },
    layout = wibox.layout.fixed.vertical,
}

local waffle_root_view = view {
    root = decorate_waffle {
        waffle_root_status_widget,
        {
            waffle_root_mpd_widget,
            draw_empty = false,
            top = panel_padding,
            widget = fixed_margin,
        },
        {
            waffle_root_agenda_widget,
            draw_empty = false,
            top = panel_padding,
            widget = fixed_margin,
        },
        {
            waffle_root_action_widget,
            draw_empty = false,
            top = panel_padding,
            widget = fixed_margin,
        },
        {
            waffle_root_admin_widget,
            draw_empty = false,
            top = panel_padding,
            widget = fixed_margin,
        },
        layout = wibox.layout.fixed.vertical,
    }
}

waffle:set_root_view(waffle_root_view)

waffle_settings_view = view {
    root = decorate_waffle(decorate_panel {
        -- button({
        --       markup = "Toggle titlebars",
        --       indicator = em("t"),
        --       key = "t",
        --       action = function (alt)
        --          if not alt then
        --             if shared.var.enable_titlebar then
        --                for _, c in ipairs(capi.client.get()) do
        --                   shared.client.titlebar_disable(c)
        --                end
        --                shared.var.enable_titlebar = false
        --             else
        --                for _, c in ipairs(capi.client.get()) do
        --                   shared.client.titlebar_enable(c)
        --                end
        --                shared.var.enable_titlebar = true
        --             end
        --          else
        --              shared.var.hide_clients_with_titlebars =
        --                  not shared.var.hide_clients_with_titlebars
        --              capi.client.emit_signal("list")
        --          end
        --          waffle:hide()
        --       end
        -- }),
        -- button({
        --       markup = "Toggle music",
        --       indicator = em("m"),
        --       key = "m",
        --       action = function (alt)
        --           mpd_widget:set_visible(not mpd_widget:get_visible())
        --           waffle:go_back()
        --       end
        -- }),
        button({
                markup = "Toggle fortune",
                indicator = em("f"),
                key = "f",
                action = function (alt)
                    shared.screen.toggle_fortune()
                end
        }),
        button({
                markup = "Cycle bar styles",
                indicator = em("b"),
                key = "b",
                action = function (alt)
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
        button({
                markup = "Wallpaper",
                indicator = em("w"),
                key = "w",
                action = function (alt)
                    shared.action.wallpaper_setup()
                    waffle:hide()
                end
        }),
        button({
                markup = "Screen layout",
                indicator = em("s"),
                key = "s",
                action = function (alt)
                    if alt then
                        local cmd = {"arandr"}
                        awful.spawn(cmd)
                    else
                        local cmd = {"rofi-screen-layout",
                                     "-normal-window",
                                     "-font", beautiful.font
                        }
                        awful.spawn(cmd)
                    end
                    waffle:hide()
                end
        }),
        layout = wibox.layout.fixed.vertical,
    }),
}


local client_waffle = view {
    root = decorate_waffle {
        decorate_panel {
            button({
                    width = dpi(48),
                    height = dpi(64),
                    button_layout = fixed_align.vertical,
                    markup = "Above",
                    indicator = em("a"),
                    key = "a",
                    action = function (alt)
                        local client = shared.waffle_selected_client
                        if not alt then
                            waffle:hide()
                        end
                        if not client.valid then
                            return
                        end
                        client.above = not client.above
                    end
            }),
            button({
                    width = dpi(48),
                    height = dpi(64),
                    button_layout = fixed_align.vertical,
                    markup = "Float",
                    indicator = em("f"),
                    key = "f",
                    action = function (alt)
                        local client = shared.waffle_selected_client
                        if not alt then
                            waffle:hide()
                        end
                        if not client.valid then
                            return
                        end
                        client.floating = not client.floating
                    end
            }),
            button({
                    width = dpi(48),
                    height = dpi(64),
                    button_layout = fixed_align.vertical,
                    markup = "Pos",
                    indicator = em("s"),
                    key = "s",
                    key_action = function (alt)
                        local client = shared.waffle_selected_client
                        waffle:hide()
                        if not client.valid then
                            return
                        end
                        shared.client.start_switcher(client, false)
                    end,
                    button_action = function (alt)
                        local client = shared.waffle_selected_client
                        waffle:hide()
                        if not client.valid then
                            return
                        end
                        if alt then
                            awful.mouse.client.resize(client, "bottom_right")
                        else
                            local geo = client:geometry()
                            mouse.coords({ x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 })
                            awful.mouse.client.move(client)
                        end
                    end
            }),
            button({
                    width = dpi(48),
                    height = dpi(64),
                    button_layout = fixed_align.vertical,
                    markup = "Max",
                    indicator = em("m"),
                    key = "m",
                    action = function (alt)
                        local client = shared.waffle_selected_client
                        if not alt then
                            waffle:hide()
                        end
                        if not client.valid then
                            return
                        end
                        client.maximized = not client.maximized
                    end
            }),
            button({
                    width = dpi(48),
                    height = dpi(64),
                    button_layout = fixed_align.vertical,
                    markup = "Close",
                    indicator = em("c"),
                    key = "c",
                    action = function (alt)
                        local client = shared.waffle_selected_client
                        waffle:hide()
                        if not client.valid then
                            return
                        end
                        client:kill()
                    end,
            }),
            layout = wibox.layout.fixed.horizontal,
        },
        spacing = dpi(10),
        layout = wibox.layout.fixed.vertical,
    },
    on_close = function()
        shared.waffle_selected_client = nil
    end
}

function shared.waffle.show_client_waffle(c, args)
    args = args or {}
    if args.anchor == false then
        local geo = c:geometry()
        args.anchor = { x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 }
    end
    shared.waffle_selected_client = c
    waffle:show(client_waffle, args)
end

return nil

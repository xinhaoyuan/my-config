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
local acolor = require("aux").color
local mpc_gobject = require("mpc-gobject")
local orgenda = require("orgenda")
local dpi = require("beautiful.xresources").apply_dpi

local waffle_width = beautiful.waffle_panel_width or dpi(240)
local button_height = beautiful.waffle_item_height or dpi(20)
local button_padding = beautiful.sep_small_size or dpi(4)
local panel_padding = beautiful.sep_big_size or dpi(10)
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

    view.widget = wibox.widget {
        root,
        margins = beautiful.useless_gap,
        widget = wibox.container.margin,
    }
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
                   forced_width = args.icon_size or button_height,
                   forced_height = args.icon_size or button_height,
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

local sep_color = gcolor(acolor(beautiful.fg_normal):blend_with(acolor(beautiful.bg_normal), 0.75):to_string())
local function decorate_panel(args)
    if args.top_sep then
        return wibox.widget {
            {
                args.widget,
                draw_empty = false,
                top = panel_padding,
                widget = fixed_margin,
            },
            bgimage = function(context, cr, width, height)
                height = panel_padding
                beautiful.draw_separator(cr, width, height)
            end,
            widget = wibox.container.background
        }
    else
        return wibox.widget.base.make_widget_from_value(args.widget)
    end
end

local function decorate_waffle(widget)
    return beautiful.apply_border_to_widget {
        widget = widget,
        top = true,
        bottom = true,
        left = true,
        right = true,
    }
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
    root = decorate_waffle(
        decorate_panel {
            widget = {
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
            }
    })
}

local waffle_settings_view
local waffle_tray_view
local waffle_tray_wrapper = wibox.widget {
    margins = beautiful.xborder_width,
    widget = wibox.container.margin
}
local function get_tray_item_key(index)
    if index < 10 then
        return tostring(index)
    elseif index < 36 then
        return string.char(87 + index)
    else
        return nil
    end
end
local function show_tray_view()
    -- waffle_tray_wrapper.widget = shared.screen.detach_tray_widget()
    -- waffle:show(waffle_tray_view, { push = true })

    local widget = { layout = wibox.layout.fixed.vertical }
    for index, info in ipairs(awesome.systray_list()) do
        table.insert(widget, button {
                         text = info[2],
                         indicator = em(get_tray_item_key(index)),
                         key = get_tray_item_key(index),
                         action = function (alt)
                             awful.spawn({'activate-tray-window', tostring(info[1]), alt and '1' or ''}, false)
                             waffle:hide()
                         end,
        })
    end

    waffle:show(view {
                    root = decorate_waffle(
                        decorate_panel {
                            widget = wibox.widget(widget)
                        }
                    )
                     },
                { push = true })
end

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

local units = {" ", "k", "m", "g", "t", "p"}
local function format_size(s)
    local ui = 1
    while s >= 1000 and ui < #units do
        ui = ui + 1
        s = s / 1000
    end
    if s >= 1000 then return "2BIG";
    else
        -- Alternative styles
        -- return string.format("%.2f%s", s, units[ui])
        local si = math.floor(s)
        return string.format("%3d.%02d%s", si, math.floor((s - si)  * 100), units[ui])
    end
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
       local mem = {}
       for line in string.gmatch(stdout,"[^\r\n]+") do
           local key, value = line:match("(.*):%s+([0-9]+) .*")
           mem[key] = value
       end
       -- Match the calculation in htop.
       local total = mem["MemTotal"]
       local cached = mem["Cached"] + mem["SReclaimable"] - mem["Shmem"]
       local used = total - mem["MemFree"] - mem["Buffers"] - cached
       local usage = math.floor(used / total * 100 + 0.5)

       local markup = "<span font_desc='" .. font_info .. "'>" .. format_size(used * 1024) .. "B</span>"
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
           awful.spawn.easy_async({"egrep", "-e", "MemTotal:|MemFree:|Buffers:|Cached:|Shmem:|SReclaimable", "/proc/meminfo"}, on_output)
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

local disk_widget_width = (waffle_width - button_height - button_padding * 4) / 2
local disk_widget
do
    local diskgraph_rd_widget = wibox.widget {
        background_color = graph_background,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local rd_text_widget = wibox.widget {
        forced_height = button_height,
        align = "center",
        point = {x = 0, y = 0},
        outline_color = beautiful.bg_normal,
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local disk_rd_widget = wibox.widget {
        diskgraph_rd_widget,
        point = {x = 0, y = 0},
        widget = wibox.container.margin
    }

    local disk_rd_layout = wibox.widget {
        {
            {
                disk_rd_widget,
                {
                    disk_rd_widget,
                    reflection = {
                        horizontal = false,
                        vertical = true,
                    },
                    widget = wibox.container.mirror,
                },
                layout = wibox.layout.fixed.vertical,
            },
            rd_text_widget,
            layout = wibox.layout.stack,
        },
        height = button_height,
        widget = wibox.container.constraint,
    }

    local diskgraph_wr_widget = wibox.widget {
        background_color = graph_background,
        forced_height = button_height / 2,
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
        color = graph_color
    }

    local wr_text_widget = wibox.widget {
        forced_height = button_height,
        align = "center",
        point = { x = 0, y = 0 },
        outline_color = beautiful.bg_normal,
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local disk_wr_widget = wibox.widget {
        diskgraph_wr_widget,
        widget = wibox.container.margin
    }

    local disk_wr_layout = wibox.widget {
        {
            {
                disk_wr_widget,
                {
                    disk_wr_widget,
                    reflection = {
                        horizontal = false,
                        vertical = true,
                    },
                    widget = wibox.container.mirror,
                },
                layout = wibox.layout.fixed.vertical,
            },
            wr_text_widget,
            layout = wibox.layout.stack,
        },
        height = button_height,
        widget = wibox.container.constraint,
    }

    disk_widget = wibox.widget {
        {
            image = gcolor.recolor_image(icons.disk, beautiful.fg_normal),
            forced_height = button_height,
            forced_width = button_height,
            widget = masked_imagebox,
        },
        {
            {
                disk_rd_layout,
                disk_wr_layout,
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

    local prev_rd = nil
    local prev_wr = nil
    local function on_output(stdout, x)
        local rd = 0
        local wr = 0
        for line in stdout:gmatch("[^\r\n]+") do
            local items = {}
            for item in line:gmatch("[^ \t:]+") do
                if #item > 0 then
                    table.insert(items, item)
                end
            end
            name = items[3]
            local test_f = io.open("/sys/block/"..name, "r")
            if test_f ~= nil then
                io.close(test_f)
                rd = rd + tonumber(items[6]) * 512
                wr = wr + tonumber(items[10]) * 512
            end
        end

        if prev_rd ~= nil then
            local rd = (rd - prev_rd) / update_interval_s
            local markup = "<span font_desc='" .. font_info .. "'>R " .. format_size(rd) .. "B/s</span>"
            rd_text_widget:set_markup(markup)
            diskgraph_rd_widget.max_value = 1024 * 1024
            diskgraph_rd_widget:add_value(rd)
        end
        prev_rd = rd
        if prev_wr ~= nil then
            local wr = (wr - prev_wr) / update_interval_s
            local markup = "<span font_desc='" .. font_info .. "'>W " .. format_size(wr) .. "B/s</span>"
            wr_text_widget:set_markup(markup)
            diskgraph_wr_widget.max_value = 1024 * 1024
            diskgraph_wr_widget:add_value(wr)
        end
        prev_wr = wr
    end

    gtimer {
        timeout = update_interval_s,
        call_now = true,
        autostart = true,
        callback = function ()
            awful.spawn.easy_async({"cat", "/proc/diskstats"}, on_output)
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

local orgenda_widget = orgenda.widget {
    font = font_info,
    item_margin = button_padding,
    width = waffle_width,
}

local waffle_root_status_widget = decorate_panel {
    widget = {
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
                key = "n",
                action = function (alt)
                    shared.action.terminal({"sudo", "iftop"})
                    waffle:hide()
                end,
            },
            button {
                label_widget = disk_widget,
                indicator = em("d"),
                key = "d",
                action = function (alt)
                    shared.action.terminal({"sudo", "iotop"})
                    waffle:hide()
                end,
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
}

local waffle_root_agenda_widget = decorate_panel {
    top_sep = true,
    widget = {
        button {
            icon = gcolor.recolor_image(icons.calendar_todo, beautiful.fg_normal),
            label_widget = wibox.widget {
                align = "center",
                format = "%Y-%m-%d %a %H:%M",
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
}

local waffle_root_action_widget = decorate_panel {
    top_sep = true,
    widget = {
        button {
            icon = gcolor.recolor_image(icons.launcher, beautiful.fg_normal),
            markup = "Launcher",
            indicator = em("r"),
            key = "r",
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
                markup = "Web",
                indicator = em("w"),
                key = "w",
                action = function (alt)
                    shared.action.web_browser()
                    waffle:hide()
                end
        }),
        button({
                icon = gcolor.recolor_image(icons.file_manager, beautiful.fg_normal),
                markup = "Files",
                indicator = em("e"),
                key = "e",
                action = function (alt)
                    shared.action.file_manager()
                    waffle:hide()
                end
        }),
        button({
                icon = gcolor.recolor_image(icons.dots, beautiful.fg_normal),
                markup = "Tray",
                indicator = em("y"),
                key = "y",
                action = function (alt)
                    show_tray_view()
                end,
        }),
        button({
                icon = gcolor.recolor_image(icons.fortune, beautiful.fg_normal),
                markup = "Fortune",
                indicator = em("f"),
                key = "f",
                action = function (alt)
                    if alt then
                        shared.screen.xkcd()
                    else
                        local fortune = shared.screen.get_fortune()
                        if fortune then
                            shared.action.web_browser("? " .. fortune)
                        end
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
}

local waffle_root_audio_widget = decorate_panel {
    top_sep = true,
    widget = button {
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
}

local waffle_root_mpd_widget = decorate_panel {
    top_sep = true,
    widget = {
        mpd_widget,
        draw_empty = false,
        layout = fixed_margin,
    },
}

local waffle_root_admin_widget = decorate_panel {
    top_sep = true,
    widget = {
        button {
            icon = gcolor.recolor_image(icons.lock, beautiful.fg_normal),
            markup = "Lock",
            indicator = em("l"),
            key = "l",
            action = function (alt)
                shared.action.screen_locker()
                waffle:hide()
            end
        },
        button {
            icon = gcolor.recolor_image(icons.poweroff, beautiful.fg_normal),
            markup = "Power",
            indicator = em("u"),
            key = "u",
            action = function (alt)
                waffle:show(waffle_shutdown_view, { push = true })
            end
        },
        layout = wibox.layout.fixed.vertical,
    }
}

local waffle_root_view = view {
    root = decorate_waffle {
        waffle_root_status_widget,
        waffle_root_audio_widget,
        waffle_root_mpd_widget,
        waffle_root_agenda_widget,
        waffle_root_action_widget,
        waffle_root_admin_widget,
        layout = wibox.layout.fixed.vertical,
    }
}

waffle:set_root_view(waffle_root_view)

waffle_settings_view = view {
    root = decorate_waffle(
        decorate_panel {
            widget = {
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
                (
                    function()
                        local b
                        b = button({
                                indicator = em("b"),
                                key = "b",
                                action = function (alt)
                                    for i, v in ipairs(beautiful.bar_styles) do
                                        if v == beautiful.bar_style then
                                            beautiful.bar_style = beautiful.bar_styles[i % #beautiful.bar_styles + 1]
                                            b.label.text = "Cycle bar style: "..beautiful.bar_style
                                            capi.screen.emit_signal("list")
                                            return
                                        end
                                    end
                                    beautiful.bar_style = "auto"
                                    capi.screen.emit_signal("list")
                                end
                        })
                        b.label.text = "Cycle bar style: "..beautiful.bar_style
                        return b
                    end
                )(),
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
            }
    }),
}

-- Unused.
waffle_tray_view = view {
    root = decorate_waffle(
        decorate_panel {
            widget = waffle_tray_wrapper
    }),
    on_close = function()
        waffle_tray_wrapper.widget = nil
        shared.screen.attach_tray_widget()
    end
}


local group_cache
local group_label = wibox.widget {
    markup = "group",
    widget = wibox.widget.textbox,
    align = "center",
}

local sticky_cache
local sticky_label = wibox.widget {
    markup = "sticky",
    widget = wibox.widget.textbox,
    align = "center",
}

local above_cache
local above_label = wibox.widget {
    markup = "above",
    widget = wibox.widget.textbox,
    align = "center",
}

local float_cache
local float_label = wibox.widget {
    markup = "float",
    widget = wibox.widget.textbox,
    align = "center",
}

local max_cache
local max_label = wibox.widget {
    markup = "max",
    widget = wibox.widget.textbox,
    align = "center",
}

local min_cache
local min_label = wibox.widget {
    markup = "min",
    widget = wibox.widget.textbox,
    align = "center",
}

local waffle_client_icon_container = wibox.widget {
    forced_width = dpi(40),
    forced_height = dpi(40),
    widget = wibox.container.constraint,
}

local update_client_waffle_labels
local client_waffle = view {
    root = decorate_waffle {
        decorate_panel {
            widget = {
                { 
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = group_label,
                            indicator = em("g"),
                            key = "g",
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                shared.client.toggle_grouping(client)
                            end
                    }),
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = sticky_label,
                            indicator = em("s"),
                            key = "s",
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.sticky = not client.sticky
                            end
                    }),
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = above_label,
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
                    layout = wibox.layout.fixed.horizontal,
                },
                {
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = min_label,
                            indicator = em("i"),
                            key = "i",
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.minimized = not client.minimized
                            end
                    }),
                    {
                        waffle_client_icon_container,
                        forced_width = dpi(64),
                        forced_height = dpi(64),
                        halign = "center",
                        valign = "center",
                        widget = wibox.container.place,
                    },
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = max_label,
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
                    layout = wibox.layout.fixed.horizontal,
                },
                {
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = float_label,
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
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            markup = "Pos",
                            indicator = em("p"),
                            key = "p",
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
                            width = dpi(64),
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
                layout = wibox.layout.fixed.vertical,
            },
        },
        spacing = button_padding,
        layout = wibox.layout.fixed.vertical,
    },
    on_close = function()
        shared.waffle_selected_client = nil
    end
}

function shared.waffle.show_client_waffle(c, args)
    args = args or {}
    if args.anchor == "client" then
        local geo = c:geometry()
        args.anchor = { x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 }
    end
    waffle:show(client_waffle, args)
    shared.waffle_selected_client = c
    waffle_client_icon_container.widget = wibox.widget {
        awful.widget.clienticon(c),
        {
            id = "default_icon",
            image = beautiful.client_default_icon,
            widget = masked_imagebox,
        },
        widget = fallback,
    }
    update_client_waffle_labels()
    capi.client.emit_signal("list")
end

update_client_waffle_labels = function ()
    if not waffle:is_in_view(client_waffle) then
        return
    end

    local now_group = (shared.waffle_selected_client and shared.waffle_selected_client.cgroup ~= nil)
    if group_cache ~= now_group then
        group_cache = now_group
        group_label:set_markup(now_group and "GROUP" or "group")
    end

    local now_sticky = (shared.waffle_selected_client and shared.waffle_selected_client.sticky)
    if sticky_cache ~= now_sticky then
        sticky_cache = now_sticky
        sticky_label:set_markup(now_sticky and "STICKY" or "sticky")
    end

    local now_above = (shared.waffle_selected_client and shared.waffle_selected_client.above)
    if above_cache ~= now_above then
        above_cache = now_above
        above_label:set_markup(now_above and "ABOVE" or "above")
    end

    local now_float = (shared.waffle_selected_client and shared.waffle_selected_client.floating)
    if float_cache ~= now_float then
        float_cache = now_float
        float_label:set_markup(now_float and "FLOAT" or "float")
    end

    local now_max = (shared.waffle_selected_client and shared.waffle_selected_client.maximized)
    if max_cache ~= now_max then
        max_cache = now_max
        max_label:set_markup(now_max and "MAX" or "max")
    end

    local now_min = (shared.waffle_selected_client and shared.waffle_selected_client.minimized)
    if min_cache ~= now_min then
        min_cache = now_min
        min_label:set_markup(now_min and "MIN" or "min")
    end
end

client.connect_signal("property::cgroup", update_client_waffle_labels)
client.connect_signal("property::sticky", update_client_waffle_labels)
client.connect_signal("property::above", update_client_waffle_labels)
client.connect_signal("property::floating", update_client_waffle_labels)
client.connect_signal("property::maximized", update_client_waffle_labels)
client.connect_signal("property::minimized", update_client_waffle_labels)

return nil

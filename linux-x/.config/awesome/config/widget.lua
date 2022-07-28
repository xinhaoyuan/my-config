local capi = {
    awesome = awesome,
    mousegrabber = mousegrabber,
}
local wibox = require("wibox")
local beautiful = require("beautiful")
local gtimer = require("gears.timer")
local awful = require("awful")
local dpi = require("beautiful.xresources").apply_dpi
local ocontainer = require("onion.container")
local opicker = require("onion.picker")
local icons = require("icons")
local masked_imagebox = require("masked_imagebox")
local outlined_textbox = require("outlined_textbox")
local fallback = require("fallback")
local gshape = require("gears.shape")
local gsurf = require("gears.surface")
local compactor = require("compactor")
local debug_container = require("debug_container")

local update_interval_s = 2
-- TODO dedup with waffle.lua
local button_padding = beautiful.sep_small_size or dpi(4)
local font_info = beautiful.font_minor

local units = {" ", "k", "m", "g", "t", "p"}
local function format_size(s, fill)
    if fill == nil then fill = true end
    local ui = 1
    while s >= 1000 and ui < #units do
        ui = ui + 1
        s = s / 1000
    end
    local si = math.floor(s)
    if s >= 1000 then return "2BIG"
    elseif s >= 100 then
        return string.format(fill and " %3d%s" or "%d%s", si, units[ui])
    elseif s >= 10 then
        return string.format(fill and "%2d.%01d%s" or "%d.%01d%s", si, math.floor((s - si) * 10), units[ui])
    elseif s >= 1 then
        return string.format("%d.%02d%s", si, math.floor((s - si) * 100), units[ui])
    elseif s > 0.001 then
        return string.format(".%03d%s", math.floor((s - si) * 1000), units[ui])
    else
        return fill and "   0 " or "0 "
    end
end

local function format_duration(s)
    return string.format("%dd%2dh%2dm", math.floor(s / 86400), math.floor(s % 86400 / 3600), math.floor(s % 3600 / 60))
end

local cpu_widget
do
   local cpu_graph_widget = wibox.widget {
      max_value = 100,
      background_color = "#00000000",
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
   }

   local cpu_text_widget = wibox.widget {
       align = "center",
       point = {x = 0, y = 0},
       outline_size = dpi(2),
       widget = outlined_textbox,
   }

   cpu_widget = wibox.widget{
       {
           {
               cpu_graph_widget,
               {
                   cpu_graph_widget,
                   reflection = {
                       horizontal = false,
                       vertical = true,
                   },
                   widget = wibox.container.mirror,
               },
               layout = wibox.layout.flex.vertical,
           },
           {
               id = "text",
               widget = cpu_text_widget
           },
           layout = wibox.layout.stack
       },
       widget = wibox.container.constraint,
   }

   local cpu_total_prev = {}
   local cpu_idle_prev = {}
   local function on_output (stdout)
       local cpu_overall_usage, cpu_max_usage, cpu_temp, cpu_freq_list
       cpu_freq_list = {}

       for line in stdout:gmatch("[^\r\n]*") do
           if line:find("^usage:") then
               local cpu_id, user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
                   line:match('(cpu%d*)%s+(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)')
               local total = user + nice + system + idle + iowait + irq + softirq + steal
               local diff_idle = idle - (cpu_idle_prev[cpu_id] or 0)
               local diff_total = total - (cpu_total_prev[cpu_id] or 0)

               if cpu_id == "cpu" then
                   cpu_overall_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10
               else
                   local cpu_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10
                   if cpu_max_usage == nil or cpu_max_usage < cpu_usage then
                       cpu_max_usage = cpu_usage
                   end
               end

               cpu_total_prev[cpu_id] = total
               cpu_idle_prev[cpu_id] = idle
           elseif line:find("^temp:") then
               local temp_str = line:match("%d+")
               cpu_temp = math.floor(tonumber(temp_str) / 1000 + 0.5)
           elseif line:find("^freq:") then
               local freq_str = line:match("[0-9.]+")
               table.insert(cpu_freq_list, tonumber(freq_str) * 1000 * 1000)
           -- else
           --     print("unknown cpu status line", line)
           end
       end

       local cpu_freq_min, cpu_freq_max
       for i = 1, #cpu_freq_list do
           if cpu_freq_min == nil or cpu_freq_min > cpu_freq_list[i] then
               cpu_freq_min = cpu_freq_list[i]
           end
           if cpu_freq_max == nil or cpu_freq_max < cpu_freq_list[i] then
               cpu_freq_max = cpu_freq_list[i]
           end
       end

       local markup = "<span font_desc='"..font_info.."'>"..
           string.format("%3d", math.floor(cpu_overall_usage)).. "%A "..
           string.format("%3d", math.floor(cpu_max_usage)).."%S "..
           string.format("%3d", math.floor(cpu_temp)).."℃ "..format_size(cpu_freq_min).."-"..format_size(cpu_freq_max)..
           "Hz</span>"
       cpu_text_widget:set_markup(markup)
       cpu_graph_widget:add_value(cpu_overall_usage)
   end

   local cpu_temp_cmd = ""
   local h = io.popen("find_coretemp_input.sh")
   if h then
       local temp_input = h:read("*a")
       if temp_input and #temp_input > 0 then
           cpu_temp_cmd =
               string.format("echo -n 'temp:'; cat %s", temp_input)
       else
           cpu_temp_cmd = ""
       end
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async_with_shell(string.format([[
grep -e "^cpu" /proc/stat | sed -e 's/^/usage:/g'
grep -e "cpu MHz" /proc/cpuinfo | sed -e 's/^/freq:/g'
%s
]], cpu_temp_cmd), on_output)
       end,
   }
end

local ram_widget
do
   local ram_graph_widget = wibox.widget {
      max_value = 100,
      background_color = "#00000000",
      step_width = dpi(2),
      step_spacing = dpi(1),
      widget = wibox.widget.graph,
   }

   local ram_text_widget = wibox.widget {
       align = "center",
       point = {x = 0, y = 0},
       outline_size = dpi(2),
       widget = outlined_textbox,
   }

   ram_widget = wibox.widget{
       {
           {
               ram_graph_widget,
               {
                   ram_graph_widget,
                   reflection = {
                       horizontal = false,
                       vertical = true,
                   },
                   widget = wibox.container.mirror,
               },
               layout = wibox.layout.flex.vertical,
           },
           {
               id = "text",
               widget = ram_text_widget
           },
           layout = wibox.layout.stack
       },
       widget = wibox.container.constraint,
   }

   local function on_output(stdout)
       local mem = {}
       local uptime

       for line in stdout:gmatch("[^\r\n]*") do
           if line:find("^mem:") then
               local key, value = line:match("mem:(.*):%s+([0-9]+) .*")
               mem[key] = value
           elseif line:find("^uptime:") then
               local uptime_str, _ = line:match("([0-9.]+) ([0-9.]+)")
               uptime = tonumber(uptime_str)
           end
       end

       -- Match the calculation in htop.
       local total = mem["MemTotal"]
       local shared = mem["Shmem"]
       local cached = mem["Cached"] + mem["SReclaimable"] - shared
       local cached_or_buffered = mem["Buffers"] + cached
       local used = total - mem["MemFree"] - cached_or_buffered - shared
       local usage = math.floor(used / total * 100 + 0.5)

       local markup = "<span font_desc='" .. font_info .. "'>"..format_size(used * 1024, false).."B "..format_size(shared * 1024, false).."B "..format_size(cached_or_buffered * 1024, false).."B "..format_duration(uptime).."</span>"
       ram_text_widget:set_markup(markup)
       ram_graph_widget:add_value(usage)
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async_with_shell([[
egrep -e "MemTotal:|MemFree:|Buffers:|Cached:|Shmem:|SReclaimable" /proc/meminfo | sed -e 's/^/mem:/g'
echo -n "uptime:"; cat /proc/uptime
]], on_output)
       end,
   }
end

local net_widget
do
    local netgraph_rx_widget = wibox.widget {
        background_color = "#00000000",
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
    }

    local rx_text_widget = wibox.widget {
        align = "center",
        point = {x = 0, y = 0},
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local net_rx_widget = wibox.widget {
        netgraph_rx_widget,
        point = {x = 0, y = 0},
        widget = wibox.container.margin
    }

    local net_rx_layout = wibox.widget{
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
                layout = wibox.layout.flex.vertical,
            },
            rx_text_widget,
            layout = wibox.layout.stack,
        },
        widget = wibox.container.constraint,
    }

    local netgraph_tx_widget = wibox.widget {
        background_color = "#00000000",
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
    }

    local tx_text_widget = wibox.widget {
        align = "center",
        point = { x = 0, y = 0 },
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
                layout = wibox.layout.flex.vertical,
            },
            tx_text_widget,
            layout = wibox.layout.stack,
        },
        widget = wibox.container.constraint,
    }

    local net_widget_icon_container = wibox.widget {
        {
            widget = masked_imagebox,
        },
        widget = ocontainer,
    }

    net_widget = wibox.widget {
        net_widget_icon_container,
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
        local has_vpn = false
        local has_wifi = false
        for line in stdout:gmatch("[^\r\n]+") do
            if line:find("^ifdev:") then
                local items = {}
                local iter = line:gmatch("[^ \t:]+"); iter() -- skip the header
                for item in iter do
                    if #item > 0 then
                        table.insert(items, item)
                    end
                end
                if items[1] == "lo" then
                    --skip
                elseif items[1]:match("^tun[0-9]*$") then
                    has_vpn = true
                else
                    -- Skips VPN for avoiding double-counting
                    recv = recv + tonumber(items[2])
                    send = send + tonumber(items[10])
                end
            elseif line:find("^wifi:") then
                has_wifi = line:find("enabled") ~= nil
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

        if has_vpn ~= net_widget.has_vpn then
            net_widget.has_vpn = has_vpn
            if has_vpn then
                net_widget_icon_container.fg_picker = opicker.beautiful{"special_", opicker.highlighted_switcher}
            else
                -- Need to investigate why nil does not work.
                net_widget_icon_container.fg_picker = nil
            end
        end

        if has_wifi ~= net_widget.has_wifi then
            net_widget.has_wifi = has_wifi
            if has_wifi then
                net_widget_icon_container.widget.image = icons.wifi
            else
                net_widget_icon_container.widget.image = icons.ethernet
            end
        end
    end

    gtimer {
        timeout = update_interval_s,
        call_now = true,
        autostart = true,
        callback = function ()
            awful.spawn.easy_async_with_shell([=[
egrep -e "[[:alnum:]]+:" /proc/net/dev | sed -e "s/^/ifdev:/g"
nmcli radio wifi | sed -e "s/^/wifi:/g"
]=], on_output)
        end,
    }
end

local disk_widget
do
    local diskgraph_rd_widget = wibox.widget {
        background_color = "#00000000",
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
    }

    local rd_text_widget = wibox.widget {
        align = "center",
        point = {x = 0, y = 0},
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    local disk_rd_widget = wibox.widget {
        diskgraph_rd_widget,
        point = {x = 0, y = 0},
        widget = wibox.container.margin
    }

    local disk_rd_layout = wibox.widget{
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
                layout = wibox.layout.flex.vertical,
            },
            rd_text_widget,
            layout = wibox.layout.stack,
        },
        widget = wibox.container.constraint,
    }

    local diskgraph_wr_widget = wibox.widget {
        background_color = "#00000000",
        step_width = dpi(2),
        step_spacing = dpi(1),
        widget = wibox.widget.graph,
        -- scale = true,
    }

    local wr_text_widget = wibox.widget {
        align = "center",
        point = { x = 0, y = 0 },
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
                layout = wibox.layout.flex.vertical,
            },
            wr_text_widget,
            layout = wibox.layout.stack,
        },
        widget = wibox.container.constraint,
    }

    disk_widget = wibox.widget {
        {
            image = icons.disk,
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

local audio_sink_widget
do
    local audio_sink_name_widget
    local audio_sink_bar_widget
    local audio_sink_buttons
    local audio_sink_toggle_mute
    local function audio_sink_choose_default()
        awful.spawn.with_shell(
            [[pactl set-default-sink "$(pacmd list-sinks | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/device.description = "([^"]*)"/,m){print id"\t"m[1]}' | rofi -dmenu -normal-window | cut -f 1)"]])
    end

    local GET_VOLUME_CMD = [[pacmd list-sinks | awk -- '/^\s*\* index/{f=1} /^\s*index/{f=0} f==1&&match($0, /device.description = "([^"]*)"$/, m){print "name="m[1]} f==1&&match($0, /^\s*volume:.*\/\s*([0-9]*)%/, m){print "volume="m[1]} f==1&&match($0,/^\s*muted:\s*(.*)$/,m){print "muted="m[1]}']]
    local SET_VOLUME_CMD = "pactl set-sink-volume @DEFAULT_SINK@"
    local INC_VOLUME_CMD = SET_VOLUME_CMD.." +5%"
    local DEC_VOLUME_CMD = SET_VOLUME_CMD.." -5%"
    local TOG_VOLUME_CMD = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

    audio_sink_name_widget = wibox.widget{
        ellipsize = "start",
        align = "center",
        valign = "center",
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    audio_sink_bar_widget = wibox.widget{
       max_value = 1,
       forced_height = dpi(2),
       border_width = 0,
       background_color = "#00000000",
       shape = gshape.bar,
       clip = true,
       widget = wibox.widget.progressbar
    }

    local update_graphic = function (stdout)
        local data = {}
        for k, v in string.gmatch(stdout, "([^\n\r]*)=([^\n\r]*)") do
            data[k] = v
        end
        if data.muted == "yes" then
            audio_sink_name_widget.text = data.name.." [MUTED]"
        else
            audio_sink_name_widget.text = data.name
        end
        audio_sink_bar_widget.value = tonumber(data.volume) / 100
    end

   local function spawn_and_update_audio_sink(cmd)
       awful.spawn.easy_async_with_shell(
           cmd .. ">/dev/null&&" .. GET_VOLUME_CMD,
           function (stdout, stderr, exitreason, exitcode)
               update_graphic(stdout, stderr, exitreason, exitcode)
           end
       )
   end

   function audio_sink_toggle_mute()
       spawn_and_update_audio_sink(TOG_VOLUME_CMD)
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async_with_shell(GET_VOLUME_CMD,
                                  function (stdout)
                                      update_graphic(stdout)
                                  end
           )
       end,
   }

   audio_sink_name_widget.keys = {
       ["-"] = function (mod, _, event)
           if event == "release" then return end
           awful.spawn.easy_async_with_shell(
               DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
       ["0"] = function (_mod, _key, event)
           if event == "release" then return end
           awful.spawn.easy_async_with_shell(
               TOG_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
       ["="] = function (_mod, _key, event)
           if event == "release" then return end
           awful.spawn.easy_async_with_shell(
               INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
   }

   audio_sink_widget = wibox.widget{
       audio_sink_name_widget,
       audio_sink_bar_widget,
       layout = wibox.layout.fixed.vertical,
   }

   audio_sink_widget:connect_signal(
       "button::press", function (_widget, x, y, b, _mods, info)
           if b == 2 then
               awful.spawn.easy_async_with_shell(
                   TOG_VOLUME_CMD..">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end)
           elseif b == 3 then
               local sx = info.drawable.drawable:geometry().x + info.x
               local width = info.width
               local value = math.floor(math.max(0, math.min(x / width, 1)) * 100 + 0.5)
               local prev_value
               local timer = gtimer{
                   timeout = 0.1,
                   autostart = true,
                   callback = function ()
                       if prev_value == value then return end
                       prev_value = value
                       awful.spawn.easy_async_with_shell(
                           SET_VOLUME_CMD.." "..tostring(value).."% >/dev/null&&" .. GET_VOLUME_CMD,
                           function (stdout, stderr, exitreason, exitcode)
                               update_graphic(stdout, stderr, exitreason, exitcode)
                           end)
                   end,
               }
               capi.mousegrabber.run(
                   function (info)
                       local p = (info.x - sx) / width
                       value = math.floor(math.max(0, math.min(p, 1)) * 100 + 0.5)
                       if not info.buttons[3] then
                           timer:stop()
                           awful.spawn.easy_async_with_shell(
                           SET_VOLUME_CMD.." "..tostring(value).."% >/dev/null&&" .. GET_VOLUME_CMD,
                           function (stdout, stderr, exitreason, exitcode)
                               update_graphic(stdout, stderr, exitreason, exitcode)
                           end)
                           return false
                       end
                       return true
                   end,
                   "sb_h_double_arrow")
           end
       end)
   function audio_sink_widget:execute(alt)
       if alt then
           -- awful.spawn({"pavucontrol"})
           return false
       else
           audio_sink_choose_default()
           return true
       end
   end
end

local audio_source_widget
do
    local audio_source_name_widget
    local audio_source_bar_widget
    local audio_source_buttons
    local audio_source_toggle_mute
    local function audio_source_choose_default()
        awful.spawn.with_shell([[pactl set-default-source "$(pacmd list-sources | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/device.description = "([^"]*)"/,m){print id"\t"m[1]}' | rofi -dmenu -normal-window | cut -f 1)"]])
    end

    local GET_VOLUME_CMD = [[pacmd list-sources | awk -- '/^\s*\* index/{f=1} /^\s*index/{f=0} f==1&&match($0, /device.description = "([^"]*)"$/, m){print "name="m[1]} f==1&&match($0, /^\s*volume:.*\/\s*([0-9]*)%/, m){print "volume="m[1]} f==1&&match($0,/^\s*muted:\s*(.*)$/,m){print "muted="m[1]}']]
    local SET_VOLUME_CMD = "pactl set-source-volume @DEFAULT_SOURCE@"
    local INC_VOLUME_CMD = SET_VOLUME_CMD.." +5%"
    local DEC_VOLUME_CMD = SET_VOLUME_CMD.." -5%"
    local TOG_VOLUME_CMD = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"

    audio_source_name_widget = wibox.widget{
        ellipsize = "start",
        align = "center",
        valign = "center",
        outline_size = dpi(2),
        widget = outlined_textbox,
    }

    audio_source_bar_widget = wibox.widget{
       max_value = 1,
       forced_height = dpi(2),
       border_width = 0,
       background_color = "#00000000",
       shape = gshape.bar,
       clip = true,
       widget = wibox.widget.progressbar
    }

    local update_graphic = function (stdout)
        local data = {}
        for k, v in string.gmatch(stdout, "([^\n\r]*)=([^\n\r]*)") do
            data[k] = v
        end
        if data.muted == "yes" then
            audio_source_name_widget.text = data.name.." [MUTED]"
        else
            audio_source_name_widget.text = data.name
        end
        audio_source_bar_widget.value = tonumber(data.volume) / 100
    end

    local function spawn_and_update_audio_source(cmd)
       awful.spawn.easy_async_with_shell(
           cmd .. ">/dev/null&&" .. GET_VOLUME_CMD,
           function (stdout, stderr, exitreason, exitcode)
               update_graphic(stdout, stderr, exitreason, exitcode)
           end
       )
   end

   function audio_source_toggle_mute()
       spawn_and_update_audio_source(TOG_VOLUME_CMD)
   end

   gtimer {
       timeout = update_interval_s,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async_with_shell(GET_VOLUME_CMD,
                                  function (stdout)
                                      update_graphic(stdout)
                                  end
           )
       end,
   }

   audio_source_name_widget.keys = {
       ["_"] = function (mod, _, event)
           if event == "release" then return end
           awful.spawn.easy_async_with_shell(
               DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
       [")"] = function (_mod, _key, event)
           if event == "press" then return end
           awful.spawn.easy_async_with_shell(
               TOG_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
       ["+"] = function (_mod, _key, event)
           if event == "release" then return end
           awful.spawn.easy_async_with_shell(
               INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
               function (stdout, stderr, exitreason, exitcode)
                   update_graphic(stdout, stderr, exitreason, exitcode)
               end
           )
       end,
   }

   audio_source_widget = wibox.widget{
       audio_source_name_widget,
       audio_source_bar_widget,
       layout = wibox.layout.fixed.vertical,
   }

   audio_source_widget:connect_signal(
       "button::press", function (_widget, x, y, b, _mods, info)
           if b == 2 then
               awful.spawn.easy_async_with_shell(
                   TOG_VOLUME_CMD..">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end)
           elseif b == 3 then
               local sx = info.drawable.drawable:geometry().x + info.x
               local width = info.width
               local value = math.floor(math.max(0, math.min(x / width, 1)) * 100 + 0.5)
               local prev_value
               local timer = gtimer{
                   timeout = 0.1,
                   autostart = true,
                   callback = function ()
                       if prev_value == value then return end
                       prev_value = value
                       awful.spawn.easy_async_with_shell(
                           SET_VOLUME_CMD.." "..tostring(value).."% >/dev/null&&" .. GET_VOLUME_CMD,
                           function (stdout, stderr, exitreason, exitcode)
                               update_graphic(stdout, stderr, exitreason, exitcode)
                           end)
                   end,
               }
               capi.mousegrabber.run(
                   function (info)
                       local p = (info.x - sx) / width
                       value = math.floor(math.max(0, math.min(p, 1)) * 100 + 0.5)
                       if not info.buttons[3] then
                           timer:stop()
                           awful.spawn.easy_async_with_shell(
                           SET_VOLUME_CMD.." "..tostring(value).."% >/dev/null&&" .. GET_VOLUME_CMD,
                           function (stdout, stderr, exitreason, exitcode)
                               update_graphic(stdout, stderr, exitreason, exitcode)
                           end)
                           return false
                       end
                       return true
                   end,
                   "sb_h_double_arrow")
           end
       end)
   function audio_source_widget:execute(alt)
       if alt then
           -- awful.spawn({"pavucontrol"})
           return false
       else
           audio_source_choose_default()
           return true
       end
   end
end

local playerctl_widget
do
    local playerctl_player_allowlist = {
        spotify = true,
        chromium = false,
        firefox = false,
    }
    local max_text_width = dpi(150)
    playerctl_widget = wibox.widget{
        {
            {
                {
                    {
                        id = "cover",
                        widget = wibox.widget.imagebox,
                    },
                    {
                        image = icons.music,
                        widget = masked_imagebox,
                    },
                    layout = fallback,
                },
                width = beautiful.bar_height,
                height = beautiful.bar_height,
                strategy = "max",
                widget = wibox.container.constraint,
            },
            widget = wibox.container.place,
        },
        {
            {
                {
                    {
                        id = "metadata",
                        wrap = "word_char",
                        widget = wibox.widget.textbox,
                    },
                    widget = compactor,
                },
                width = max_text_width,
                height = beautiful.bar_height,
                strategy = "max",
                widget = wibox.container.constraint,
            },
            widget = wibox.container.place,
        },
        spacing = beautiful.sep_small_size,
        layout = wibox.layout.fixed.horizontal,
        visible = false
    }

    local player
    local fetching_cover = nil
    local function fetch_cover(url)
        if fetching_cover == url then return end
        playerctl_widget:get_children_by_id("cover")[1].image = nil
        fetching_cover = url
        -- print("fetch", url)
        awful.spawn.easy_async(
            {"curl", url, "-o", "/tmp/playerctl-cover", "-s", "-w", "%{content_type}"},
            function (stdout, stderr, exit_reason, exit_code)
                if fetching_cover ~= url or exit_reason ~= "exit" or exit_code ~= 0 then return end
                -- print(stdout)
                playerctl_widget:get_children_by_id("cover")[1].image = gsurf.load_uncached("/tmp/playerctl-cover")
            end)
    end

    local pending_info = {}
    local function handle_playerctl_line(line)
        if line == "END" then
            if not playerctl_player_allowlist[pending_info.player] then return end
            if pending_info.status == "Stopped" then
                playerctl_widget.visible = false
                player = nil
                return
            else
                playerctl_widget.visible = true
            end
            player = pending_info.player
            local text
            -- For youtube
            if pending_info.artist then pending_info.artist = pending_info.artist:gsub(" %- Topic$", "") end
            -- for k, v in pairs(pending_info) do print(">", k, v) end
            for _, t in ipairs({"title", "artist", "album"}) do
                if pending_info[t] and #pending_info[t] > 0 then
                    text = (text and (text.." - ") or "")..pending_info[t]
                end
            end
            playerctl_widget:get_children_by_id("metadata")[1].text = text
            if pending_info.cover then
                fetch_cover(pending_info.cover)
            end
            pending_info = {}
            return
        end
        local k, v = line:match("([^=]*)=(.*)")
        if k then
            pending_info[k] = v
        end
    end
    local format = "player={{playerName}}\nstatus={{status}}\ntitle={{title}}\nalbum={{album}}\ncover={{mpris:artUrl}}\nartist={{artist}}\nEND"
    local pid = awful.spawn.with_line_callback(
        {"playerctl", "-a", "-f", format, "metadata", "-F"},
        {stdout = handle_playerctl_line})
    capi.awesome.connect_signal("exit", function () awful.spawn({"kill", tostring(pid)}, false) end)
    gtimer{
        timeout = update_interval_s,
        autostart = true,
        callback = function ()
            if player == nil then return end
            awful.spawn.easy_async(
                {"playerctl", "-a", "-f", format, "metadata"},
                function (stdout, ...)
                    local old_pending_info = pending_info
                    pending_info = {}
                    player = nil
                    for line in stdout:gmatch("[^\n\r]+") do
                        handle_playerctl_line(line)
                    end
                    pending_info = old_pending_info
                    if player == nil then
                        playerctl_widget.visible = false
                    end
                end)

        end,
    }
    playerctl_widget:connect_signal(
        "button::release", function (_w, _x, _y , b)
            if b == 1 then
                awful.spawn({"playerctl", "-p", player, "play-pause"}, false)
            elseif b == 2 then
                awful.spawn.with_shell(string.format("playerctl -p %s pause; playerctl -p %s stop", player, player))
            elseif b == 3 then
                awful.spawn({"playerctl", "-p", player, "next"}, false)
            end
        end)
end

return {
    cpu_widget = cpu_widget,
    ram_widget = ram_widget,
    net_widget = net_widget,
    disk_widget = disk_widget,
    audio_sink_widget = audio_sink_widget,
    audio_source_widget = audio_source_widget,
    playerctl_widget = playerctl_widget,
}

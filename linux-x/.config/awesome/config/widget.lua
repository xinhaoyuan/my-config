local capi = {
    awesome = awesome,
    mousegrabber = mousegrabber,
}
local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local awful = require("awful")
local beautiful = require("beautiful")
local compactor = require("compactor")
local debug_container = require("debug_container")
local dpi = require("beautiful.xresources").apply_dpi
local fallback = require("fallback")
local fixed_align = require("fixed_align")
local fixed_place = require("fixed_place")
local gshape = require("gears.shape")
local gsurf = require("gears.surface")
local gtimer = require("gears.timer")
local icons = require("icons")
local masked_imagebox = require("masked_imagebox")
local mycalendar = require("my-calendar")
local orgenda = require("orgenda")
local outlined_textbox = require("outlined_textbox")
local prism = require("prism")
local regulator = require("regulator")
local wibox = require("wibox")
local scroller = require("scroller")

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

       local markup = string.format(
           "<span font_desc='%s'>%3d%%A%3d%%S%3d℃ %s-%sHz</span>",
           font_info,
           math.floor(cpu_overall_usage), math.floor(cpu_max_usage),
           math.floor(cpu_temp),
           format_size(cpu_freq_min),
           format_size(cpu_freq_max))
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
            {
                widget = masked_imagebox,
            },
            draw_pickers = {
                prism.picker.list{"fg", prism.picker.branch{
                    "has_vpn",
                    prism.picker.beautiful{"special_", prism.picker.highlighted_switcher}}},
            },
            widget = prism.wrap(wibox.container.background),
        },
        widget = prism.layer,
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
            net_widget_icon_container.context_transformation = {
                has_vpn = has_vpn
            }
            net_widget.has_vpn = has_vpn
        end

        if has_wifi ~= net_widget.has_wifi then
            net_widget.has_wifi = has_wifi
            if has_wifi then
                net_widget_icon_container.widget.widget.image = icons.wifi
            else
                net_widget_icon_container.widget.widget.image = icons.ethernet
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

local audio_widget_cache = {
    sink = setmetatable({}, {__mode = "v"}),
    source = setmetatable({}, {__mode = "v"}),
}
local function get_audio_widget(name, device_type)
    if audio_widget_cache[device_type] == nil then return end
    if audio_widget_cache[device_type][name] then return audio_widget_cache[device_type][name] end

--     local buttons
--     local toggle_mute
--     local SET_VOLUME_CMD = "pactl set-sink-volume "..name
--     local INC_VOLUME_CMD = SET_VOLUME_CMD.." +5%"
--     local DEC_VOLUME_CMD = SET_VOLUME_CMD.." -5%"
--     local TOG_VOLUME_CMD = "pactl set-sink-mute "..name.." toggle"

    local widget = wibox.widget{
        nil,
        {
            id = "name",
            ellipsize = "start",
            align = "center",
            valign = "center",
            outline_size = dpi(2),
            widget = outlined_textbox,
        },
        {
            id = "volume",
            max_value = 1,
            forced_height = dpi(2),
            border_width = 0,
            background_color = "#00000000",
            shape = gshape.bar,
            clip = true,
            widget = wibox.widget.progressbar
        },
        layout = fixed_align.vertical
    }

--    local function spawn_and_update_audio_sink(cmd)
--        awful.spawn.easy_async_with_shell(
--            cmd .. ">/dev/null&&" .. GET_VOLUME_CMD,
--            function (stdout, stderr, exitreason, exitcode)
--                update_graphic(stdout, stderr, exitreason, exitcode)
--            end
--        )
--    end

--    function audio_sink_toggle_mute()
--        spawn_and_update_audio_sink(TOG_VOLUME_CMD)
--    end

--    -- For waffle.
--    widget.keys = {
--        ["-"] = function (mod, _, event)
--            if event == "release" then return end
--            awful.spawn.easy_async_with_shell(
--                DEC_VOLUME_CMD,
--                function (...)
--                    print("dec vol:", ...)
--                end
--            )
--        end,
--        ["0"] = function (_mod, _key, event)
--            if event == "release" then return end
--            awful.spawn.easy_async_with_shell(
--                TOG_VOLUME_CMD,
--                function (...)
--                    print("tog vol:", ...)
--                end
--            )
--        end,
--        ["="] = function (_mod, _key, event)
--            if event == "release" then return end
--            awful.spawn.easy_async_with_shell(
--                INC_VOLUME_CMD,
--                function (...)
--                    print("inc vol:", ...)
--                end
--            )
--        end,
--    }

--    -- For bento.
--    function widget:handle_key(mods, key, event)
--        if event ~= "press" then return end
--        if self.keys[key] then
--            self.keys[key](mods, key, event)
--            return true
--        end
--        return false
--    end

--    widget:connect_signal(
--        "button::press", function (_widget, x, y, b, _mods, info)
--            if b == 2 then
--                awful.spawn.easy_async_with_shell(
--                    TOG_VOLUME_CMD..">/dev/null&&" .. GET_VOLUME_CMD,
--                    function (stdout, stderr, exitreason, exitcode)
--                        update_graphic(stdout, stderr, exitreason, exitcode)
--                    end)
--            elseif b == 3 then
--                local sx = info.drawable.drawable:geometry().x + info.x
--                local sy = info.drawable.drawable:geometry().y + info.y
--                local width = info.width
--                local value = math.floor(math.max(0, math.min(x / width, 1)) * 100 + 0.5)
--                local prev_value
--                local timer = gtimer{
--                    timeout = 0.1,
--                    autostart = true,
--                    callback = function ()
--                        if prev_value == value then return end
--                        prev_value = value
--                        awful.spawn.easy_async_with_shell(
--                            SET_VOLUME_CMD.." "..tostring(value),
--                            function (stdout, stderr, exitreason, exitcode)
--                                print("set vol:", ...)
--                            end)
--                    end,
--                }
--                local init_info = info
--                capi.mousegrabber.run(
--                    function (info)
--                        local p = (info.x - sx) / width
--                        value = math.floor(math.max(0, math.min(p, 1)) * 100 + 0.5)
--                        if not info.buttons[3] then
--                            timer:stop()
--                            awful.spawn.easy_async_with_shell(
--                                SET_VOLUME_CMD.." "..tostring(value),
--                                function (stdout, stderr, exitreason, exitcode)
--                                    print("set vol:", ...)
--                                end)
--                            audio_sink_widget:emit_signal_recursive("button::release", init_info.x, init_info.y, 3, {}, init_info)
--                            return false
--                        end
--                        return true
--                    end,
--                    "sb_h_double_arrow")
--            elseif b == 4 then
--                awful.spawn.easy_async_with_shell(
--                    INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
--                    function (stdout, stderr, exitreason, exitcode)
--                        update_graphic(stdout, stderr, exitreason, exitcode)
--                    end
--                )
--            elseif b == 5 then
--                awful.spawn.easy_async_with_shell(
--                    DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
--                    function (stdout, stderr, exitreason, exitcode)
--                        update_graphic(stdout, stderr, exitreason, exitcode)
--                    end
--                )
--            end
--        end)
--    function audio_sink_widget:execute(alt)
--        if alt then
--            -- awful.spawn({"pavucontrol"})
--            return false
--        else
--            audio_sink_choose_default()
--            return true
--        end
--    end

end

local audio_sink_widget
do
    local audio_sink_name_widget
    local audio_sink_bar_widget
    local audio_sink_buttons
    local audio_sink_toggle_mute
    local function audio_sink_choose_default()
        awful.spawn.with_shell(
            [[pactl set-default-sink "$(pacmd list-sinks | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/Description:\s*(.*)$/,m){print id"\t"m[1]}' | rofi -dmenu -normal-window | cut -f 1)"]])
    end

    local GET_VOLUME_CMD = [[DEFAULT="$(pactl get-default-sink)"; pactl list sinks | awk -- 'match($0, /Name:\s*(.*)/, m){f=(m[1]=="'"$DEFAULT"'")} f&&match($0, /Description:\s*(.*)$/, m){print "name="m[1]} f&&match($0, /^\s*Volume:.*\/\s*([0-9]*)%/, m){print "volume="m[1]} f&&match($0,/^\s*Mute:\s*(.*)$/,m){print "muted="m[1]}']]
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
       nil,
       audio_sink_name_widget,
       audio_sink_bar_widget,
       layout = fixed_align.vertical
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
               local sy = info.drawable.drawable:geometry().y + info.y
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
               local init_info = info
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
                           audio_sink_widget:emit_signal_recursive("button::release", init_info.x, init_info.y, 3, {}, init_info)
                           return false
                       end
                       return true
                   end,
                   "sb_h_double_arrow")
           elseif b == 4 then
               awful.spawn.easy_async_with_shell(
                   INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end
               )
           elseif b == 5 then
               awful.spawn.easy_async_with_shell(
                   DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end
               )
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
        awful.spawn.with_shell([[pactl set-default-source "$(pacmd list-sources | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/Description:\s*(.*)$/,m){print id"\t"m[1]}' | rofi -dmenu -normal-window | cut -f 1)"]])
    end

    local GET_VOLUME_CMD = [[DEFAULT="$(pactl get-default-source)"; pactl list sources | awk -- 'match($0, /Name:\s*(.*)/, m){f=(m[1]=="'"$DEFAULT"'")} f&&match($0, /Description:\s*(.*)$/, m){print "name="m[1]} f&&match($0, /^\s*Volume:.*\/\s*([0-9]*)%/, m){print "volume="m[1]} f&&match($0,/^\s*Mute:\s*(.*)$/,m){print "muted="m[1]}']]
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
       nil,
       audio_source_name_widget,
       audio_source_bar_widget,
       layout = fixed_align.vertical,
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
               local sy = info.drawable.drawable:geometry().y + info.y
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
               local init_info = info
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
                           audio_source_widget:emit_signal_recursive("button::release", sx, sy, 3, {}, init_info)
                           return false
                       end
                       return true
                   end,
                   "sb_h_double_arrow")
           elseif b == 4 then
               awful.spawn.easy_async_with_shell(
                   INC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end
               )
           elseif b == 5 then
               awful.spawn.easy_async_with_shell(
                   DEC_VOLUME_CMD .. ">/dev/null&&" .. GET_VOLUME_CMD,
                   function (stdout, stderr, exitreason, exitcode)
                       update_graphic(stdout, stderr, exitreason, exitcode)
                   end
               )
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
    playerctl_widget = wibox.widget{
        {
            {
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
                        widget = regulator,
                    },
                    widget = wibox.container.place,
                },
                right = beautiful.sep_small_size,
                widget = wibox.container.margin,
            },
            {
                {
                    {
                        id = "metadata",
                        wrap = "word_char",
                        align = "center",
                        widget = wibox.widget.textbox,
                    },
                    widget = compactor,
                },
                -- halign = "left",
                widget = wibox.container.place,
            },
            layout = fixed_align.horizontal,
        },
        id = "container",
        widget = wibox.container.background,
        visible = false,
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
                playerctl_widget:emit_signal("property::visible")
                return
            else
                playerctl_widget.visible = true
                playerctl_widget:emit_signal("property::visible")
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
            playerctl_widget:get_children_by_id("container")[1].opacity =
                pending_info.status == "Playing" and 1 or 0.5
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
    if type(pid) == "string" then
        print("Got error message while spawning playerctl: "..pid)
    else
        capi.awesome.connect_signal(
            "exit", function ()
                capi.awesome.kill(pid, capi.awesome.unix_signal.SIGTERM)
            end)
    end
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
                        playerctl_widget:emit_signal("property::visible")
                    end
                end)

        end,
    }
    function playerctl_widget:player_pause()
        awful.spawn({"playerctl", "-p", player, "play-pause"}, false)
    end
    function playerctl_widget:player_stop()
        awful.spawn.with_shell(string.format("playerctl -p %s pause; playerctl -p %s stop", player, player))
    end
    function playerctl_widget:player_next()
        awful.spawn({"playerctl", "-p", player, "next"}, false)
    end
    playerctl_widget:connect_signal(
        "button::release", function (w, _x, _y , b)
            if b == 1 then
                w:player_pause()
            elseif b == 2 then
                w:player_stop()
            elseif b == 3 then
                w:player_next()
            end
        end)
    playerctl_widget.keys = {
       ["m"] = function (mod, _, event)
           if event == "release" then return end
           playerctl_widget:player_pause()
       end,
       ["M"] = function (mod, _, event)
           if event == "release" then return end
           playerctl_widget:player_next()
       end,
   }

end

local cal_widget
cal_widget = wibox.widget{
    date = os.date('*t'),
    -- custom property.
    today = os.date('*t'),
    -- custom property.
    active_dates = {},
    font = beautiful.font_mono,
    week_numbers = true,
    start_sunday = true,
    long_weekdays = true,
    spacing = 0,
    widget = mycalendar.month
}
-- Has to set it after the properties.
function cal_widget.fn_embed(widget, flag, date)
    local active_dates = cal_widget.active_dates
    local today = cal_widget.today
    if flag == "header" then
        local sign
        if date.year == today.year then
            if date.month == today.month then
                sign = 0
            else
                sign = date.month < today.month and -1 or 1
            end
        else
            sign = date.year < today.year and -1 or 1
        end
        widget.font = beautiful.font_name_normal..' 12'
        widget = wibox.widget{
            sign > 0 and {
                text = " <<",
                align = "left",
                font = widget.font,
                widget = wibox.widget.textbox,
            },
            widget,
            sign < 0 and {
                text =  ">> ",
                align = "right",
                font = widget.font,
                widget = wibox.widget.textbox,
            },
            expand = "outside",
            layout = wibox.layout.align.horizontal,
        }
        return widget
    elseif flag == "month" then
        return widget
    elseif flag == "weekday" or flag == "weeknumber" then
        widget.font = font_info
        widget = wibox.widget{
            widget,
            draw_pickers = {
                fg = prism.picker.beautiful{"minor_", prism.picker.highlighted_switcher},
            },
            widget = prism.container.background,
        }
    end

    local inverted = false

    if flag == "normal" or flag == "focus" then
        if today.year == date.year and today.month == date.month and today.day == date.day then
            inverted = true
        end

        if active_dates[date.year] and active_dates[date.year][date.month] and active_dates[date.year][date.month][date.day] then
            widget = wibox.widget {
                widget,
                draw_pickers = {
                    fg = prism.picker.beautiful{
                        "special_", prism.picker.branch{"inverted", "focus", "normal"}},
                },
                widget = prism.container.background,
            }
        end
    end
    widget = wibox.widget{
        widget,
        halign = "center",
        widget = wibox.container.place,
    }

    if inverted then
        return wibox.widget{
            {
                {
                    widget,
                    margins = dpi(2),
                    widget = wibox.container.margin
                },
                shape = function (cr, width, height)
                    if beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width then
                        beautiful.rect_with_corners(cr, width, height, true, true, true, true,
                                                    beautiful.xborder_radius - beautiful.xborder_width)
                    else
                        beautiful.rect_with_corners(cr, width, height)
                    end
                end,
                draw_pickers = {
                    fg = prism.picker.beautiful{"fg_focus"},
                    bg = prism.picker.beautiful{"bg_focus"},
                },
                widget = prism.container.background,
            },
            context_transformation = {inverted = true},
            widget = prism.layer,
        }
    else
        return wibox.widget {
            widget,
            margins = dpi(2),
            widget = wibox.container.margin
        }
    end
end

function cal_widget:move_date(delta)
    local date = cal_widget:get_date()
    if delta.day ~= nil then date.day = date.day + delta.day end
    if delta.month ~= nil then date.month = date.month + delta.month end
    if delta.year ~= nil then date.year = date.year + delta.year end
    cal_widget:set_date(nil)
    cal_widget:set_date(date)
end

function cal_widget:reset_to_today()
    self:set_date(nil)
    self:set_date(cal_widget.today)
end

function cal_widget:refresh()
    self:move_date{}
end

cal_widget:connect_signal(
    "button::press",
    function (self, x, y, button)
        if button == 2 then
            self:reset_to_today()
        elseif button == 1 or button == 4 then
            self:move_date{month = -1}
        elseif button == 3 or button == 5 then
            self:move_date{month = 1}
        end
    end)

gtimer {
    timeout = 10,
    autostart = true,
    call_now = true,
    callback = function()
        local new_today = os.date("*t")
        if cal_widget.today.year ~= new_today.year or
            cal_widget.today.month ~= new_today.month or
            cal_widget.today.day ~= new_today.day then
            cal_widget.today = new_today
            cal_widget:refresh()
        end
    end
}

orgenda.data:connect_signal(
    "property::items",
    function ()
        local active_dates = {}
        for _, item in ipairs(orgenda.data.items) do
            if item.timestamp and not item.done then
                local date = os.date("*t", item.timestamp)
                local y = active_dates[date.year] or {}
                local m = y[date.month] or {}
                m[date.day] = true
                y[date.month] = m
                active_dates[date.year] = y
            end
        end
        cal_widget.active_dates = active_dates
        cal_widget:refresh()
    end
)

local orgenda_widget
local orgenda_items_widget
do
    local orgenda_header
    orgenda_header = wibox.widget{
        {
            {
                nil,
                {
                    markup = "<span size='large'>TODO</span>",
                    widget = wibox.widget.textbox,
                },
                {
                    {
                        {
                            image = icons.refresh,
                            resize = true,
                            forced_height = button_height,
                            forced_width = button_height,
                            widget = masked_imagebox,
                        },
                        margins = 0,
                        widget = wibox.container.margin,
                    },
                    halign = "right",
                    widget = wibox.container.place,
                },
                expand = "outside",
                layout = wibox.layout.align.horizontal,
            },
            draw_pickers = {
                fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                bg = prism.picker.beautiful{"bg_", prism.picker.highlighted_switcher},
            },
            widget = prism.container.background,
        },
        context_transformation = {highlighted = false},
        widget = prism.layer,
    }
    orgenda_header:connect_signal(
        "mouse::enter",
        function ()
            orgenda_header.context_transformation = {highlighted = true}
        end
    )
    orgenda_header:connect_signal(
        "mouse::leave",
        function ()
            orgenda_header.context_transformation = {highlighted = false}
        end
    )
    orgenda_header:connect_signal(
        "button::release",
        function ()
            orgenda.schedule_reset()
        end
    )

    local organda_color_func = function (priority, done)
        local key = "orgenda_color_p"..tostring(priority)..(done and "_done" or "_todo")
        return beautiful[key]
    end
    function orgenda_get_icon(item)
        return beautiful["orgenda_icon_p"..tostring(item.priority).."_"..(item.done and "done" or "todo")]
    end
    -- local
    orgenda_items_widget = orgenda.widget{
        create_item_widget_cb = function ()
            local widget = wibox.widget{
                {
                    {
                        {
                            {
                                {
                                    id = "icon_role",
                                    {
                                        forced_width = beautiful.icon_size,
                                        forced_height = beautiful.icon_size,
                                        widget = masked_imagebox,
                                    },
                                    widget = prism.container.background,
                                },
                                valign = "top",
                                widget = wibox.container.place
                            },
                            right = beautiful.sep_small_size,
                            widget = wibox.container.margin,
                        },
                        {
                            {
                                {
                                    id = "timestamp_role",
                                    widget = wibox.widget.textbox
                                },
                                {
                                    {
                                        id = "text_role",
                                        ellipsize = "none",
                                        align = "left",
                                        valign = "center",
                                        wrap = "word_char",
                                        widget = wibox.widget.textbox
                                    },
                                    fill_horizontal = true,
                                    content_fill_horizontal = true,
                                    widget = wibox.container.place,
                                },
                                layout = wibox.layout.fixed.vertical
                            },
                            valign = "center",
                            widget = wibox.container.place,
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    draw_pickers = {
                        fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                        prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                    },
                    widget = prism.container.background,
                },
                context_transformation = {highlighted = false},
                widget = prism.layer,
            }
            function widget:set_focused(f)
                self._private.focused = f
                self.context_transformation = {highlighted = f}
            end
            function widget:execute()
                if self._private.focused then
                    orgenda.toggle_done(self.item)
                end
            end
            widget:connect_signal(
                "button::release",
                function (self, _x, _y, button)
                    if button == 2 then
                        orgenda.hide(self.item)
                    elseif button == 3 then
                        orgenda.promote(self.item)
                    end
                end
            )
            return widget
        end,
        update_item_widget_cb = function (widget, item)
            widget:get_children_by_id("icon_role")[1].widget.image = orgenda_get_icon(item)
            widget:get_children_by_id("icon_role")[1].draw_pickers = {
                fg = prism.picker.wrap{
                    organda_color_func,
                    item.priority,
                    item.done,
                },
            }
            widget:get_children_by_id("text_role")[1].markup = item.decorated_text
            widget.item = item
        end,
    }
    -- local
    orgenda_widget = wibox.widget{
        {
            orgenda_header,
            {
                {
                    orgenda_items_widget,
                    widget = scroller,
                },
                {
                    {
                        markup = "<span size='large' foreground='"..beautiful.minor_normal.."'>Wow!\nNo TODOs!\nSo clean!</span>",
                        align = "center",
                        widget = wibox.widget.textbox,
                    },
                    widget = wibox.container.place,
                },
                widget = fallback,
            },
            layout = wibox.layout.align.vertical,
        },
        fill_vertical = true,
        content_fill_vertical = true,
        valign = "top",
        widget = fixed_place,
    }

    orgenda_widget.visible = shared.vars.show_notes
    shared.vars:connect_signal(
        "property::show_notes",
        function(_, value)
            orgenda_widget.visible = value
        end
    )
end

return {
    cpu_widget = cpu_widget,
    ram_widget = ram_widget,
    net_widget = net_widget,
    disk_widget = disk_widget,
    audio_sink_widget = audio_sink_widget,
    audio_source_widget = audio_source_widget,
    playerctl_widget = playerctl_widget,
    cal_widget = cal_widget,
    orgenda_widget = orgenda_widget,
    orgenda_items_widget = orgenda_items_widget,
}

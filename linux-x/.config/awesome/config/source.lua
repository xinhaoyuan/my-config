local capi = {
    awesome = awesome,
}
local cwidget = require((...):match("(.-)[^%.]+$") .. "widget")
local source = require("awemni.source")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local lgi = require("lgi")
local gio = lgi.Gio
local gtk = lgi.require("Gtk", "3.0")
local dpi = require("beautiful.xresources").apply_dpi
local wibox = require("wibox")
local fixed_align = require("fixed_align")
local prism = require("prism")
local awful = require("awful")
local gcolor = require("gears.color")
local notix = require("notix")
local waffle = require("waffle")
local glib = require("lgi").GLib
local csv = require("csv")

local gtk_icon_theme = gtk.IconTheme.get_default()
-- Force build the cache.
gtk_icon_theme:load_icon(gtk_icon_theme:get_example_icon_name(), beautiful.icon_size, 0)

local cached_execution_path = gfs.get_cache_dir() .. "/history_app_execution"
local cached_execution = {}
local function restore_cache_execution()
    cached_execution = {}
    local f, err = io.open(cached_execution_path, "r")
    if err then return end
    local keyword
    local timestamp
    for line in f:lines() do
        if keyword == nil then
            keyword = line
        elseif timestamp == nil then
            timestamp = tonumber(line)
        else
            cached_execution[line] = {timestamp, keyword}
            keyword = nil
            timestamp = nil
        end
    end
    f:close()
end
restore_cache_execution()

local function cache_execution(name, input)
    cached_execution[name] = {
        glib.get_real_time(),
        input or (cached_execution[name] and cached_execution[name][2]) or "",
    }
end

local function save_cache_execution()
    local f, err = io.open(cached_execution_path, "w")
    if err then
        return
    end
    for k, v in pairs(cached_execution) do
        f:write(string.format("%s\n%d\n%s\n", v[2], v[1], k))
    end
    f:close()
end
capi.awesome.connect_signal("exit", save_cache_execution)

local function get_apps()
    local apps = {}
    for _, ai in ipairs(gio.AppInfo.get_all()) do
        if ai:should_show() then
            apps[#apps + 1] = {
                ai = ai,
                name = ai:get_name(),
                display_name = ai:get_display_name(),
            }
        end
    end
    table.sort(apps, function (a, b)
                   local ac = cached_execution[a.name]
                   local bc = cached_execution[b.name]
                   if ac and bc then return ac[1] > bc[1] end
                   if ac or bc then return bc == nil end
                   return a.name < b.name
               end)
    local apps_source
    apps_source = source.filterable{
        upstream = source.array_proxy{
            array = apps,
        },
        callbacks = {
            pre_filter = function (input)
                apps.focus = nil
                local keyword_assignment, keyword_search
                if input then
                    keyword_assignment = input:match("^([^:]*):")
                    keyword_search = input:match("([^:]+):?$")
                end
                return {
                    assignment = keyword_assignment,
                    search = keyword_search,
                }
            end,
            filter = function (filter, entry)
                if filter == nil or filter.search == nil then return 0 end
                local cache_entry = cached_execution[entry.name]
                if cache_entry and cache_entry[2] == filter.search and #filter.search > 0 then return 0 end
                local succ, start = pcall(string.find, entry.display_name:lower(), filter.search:lower())
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
                for i, e in ipairs(filtered_results) do
                    if apps.focused_element == apps[e[1]] then
                        apps.focus = i
                    end
                end
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local icon_path
                if gtk_icon_theme then
                    repeat
                        local icon = e.ai:get_icon()
                        if icon == nil then break end
                        local icon_info = gtk_icon_theme:lookup_by_gicon(icon, beautiful.icon_size, 0)
                        if icon_info == nil then break end
                        icon_path = icon_info:get_filename()
                    until true
                end
                local w = wibox.widget{
                    {
                        {
                            {
                                {
                                    {
                                        image = icon_path,
                                        forced_width = beautiful.icon_size,
                                        forced_height = beautiful.icon_size,
                                        widget = wibox.widget.imagebox,
                                    },
                                    right = beautiful.sep_small_size,
                                    widget = wibox.container.margin,
                                },
                                {
                                    {
                                        {
                                            text = e.display_name,
                                            widget = wibox.widget.textbox,
                                        },
                                        {
                                            id = "desc",
                                            {
                                                {
                                                    font = font_info,
                                                    text = e.ai:get_description(),
                                                    widget = wibox.widget.textbox,
                                                },
                                                height = 60,
                                                strategy = "max",
                                                widget = wibox.container.constraint,
                                            },
                                            top = dpi(2),
                                            left = dpi(2),
                                            right = dpi(2),
                                            draw_empty = false,
                                            visible = false,
                                            widget = wibox.container.margin,
                                        },
                                        layout = wibox.layout.fixed.vertical,
                                    },
                                    valign = "center",
                                    halign = "left",
                                    widget = wibox.container.place,
                                },
                                layout = fixed_align.horizontal,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    cache_execution(e.name, apps_source.filter and apps_source.filter.assignment)
                    e.ai:launch()
                    waffle:hide()
                end
                function w:set_focused(v)
                    w:get_children_by_id("desc")[1].visible = v
                    self.context_transformation = {highlighted = v}
                    if v then apps.focused_element = e end
                end
                e.widget = w
                return w
            end,
        },
    }
    return apps_source
end

local function get_audio_sinks()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local w = wibox.widget{
                    {
                        {
                            {
                                text = e.name,
                                widget = wibox.widget.textbox,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    awful.spawn({"pactl", "set-default-sink", tostring(e.id)}, false)
                    waffle:hide()
                end
                function w:set_focused(v)
                    self.context_transformation = {highlighted = v}
                end
                e.widget = w
                return w
            end,
        },
    }
    awful.spawn.easy_async_with_shell(
        [[pactl --format=json list sinks | jq -r '.[] | [.name, .description, .mute, ([.volume[] | .value_percent | match("[0-9]*") | .string | tonumber] | select(length > 0) | add / length)] | @csv']],
        function (stdout, _stderr, _exitreason, _exitcode)
            for _, row in csv.rows_iterator(stdout) do
                output:append{id = row[1], name = row[2], muted = string.lower(row[3]) == "true", volume = tonumber(row[4])}
            end
        end
    )
    return ret
end

local function get_audio_sources()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local w = wibox.widget{
                    {
                        {
                            {
                                text = e.name,
                                widget = wibox.widget.textbox,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    awful.spawn({"pactl", "set-default-source", tostring(e.id)}, false)
                    waffle:hide()
                end
                function w:set_focused(v)
                    self.context_transformation = {highlighted = v}
                end
                e.widget = w
                return w
            end,
        },
    }
    awful.spawn.easy_async_with_shell(
        [[pactl --format=json list sources | jq -r '.[] | [.name, .description, .mute, ([.volume[] | .value_percent | match("[0-9]*") | .string | tonumber] | select(length > 0) | add / length)] | @csv']],
        function (stdout, _stderr, _exitreason, _exitcode)
            for _, row in csv.rows_iterator(stdout) do
                output:append{id = row[1], name = row[2], muted = string.lower(row[3]) == "true", volume = tonumber(row[4])}
            end
        end
    )
    return ret
end

local function get_autorandr_config(contents)
    local outputs = {}
    local current_output_name
    for line in contents:gmatch("[^\n\r]+") do
        repeat
            local output = line:match("^output%s+(.-)$")
            if output then
                current_output_name = output
                break
            end
            local mode_w, mode_h = line:match("^mode%s+([0-9]+)x([0-9]+)$")
            if current_output_name and mode_w and mode_h then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].width = mode_w
                outputs[current_output_name].height = mode_h
                break
            end
            local pos_x, pos_y = line:match("^pos%s+([0-9]+)x([0-9]+)$")
            if current_output_name and pos_x and pos_y then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].x = pos_x
                outputs[current_output_name].y = pos_y
                break
            end
            local rotate = line:match("^rotate%s+(.-)$")
            if current_output_name and rotate then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].rotate = rotate
                break
            end
        until true
    end
    local overall_width, overall_height
    for k, v in pairs(outputs) do
        if v.rotate == "left" or v.rotate == "right" then
            v.width, v.height = v.height, v.width
        end
        if overall_width == nil or v.x + v.width > overall_width then
            overall_width = v.x + v.width
        end
        if overall_height == nil or v.y + v.height > overall_height then
            overall_height = v.y + v.height
        end
    end
    return {
        overall_width = overall_width,
        overall_height = overall_height,
        outputs = outputs,
    }
end

local rotate_label = {
    normal = "",
    left = "←",
    right = "→",
    inverted = "↓",
}
local function draw_autorandr_config(config, cr, width, height)
    local scale
    if width / config.overall_width < height / config.overall_height then
        scale = width / config.overall_width
        cr:translate(0, (height - config.overall_height * scale) / 2)
    else
        scale = height / config.overall_height
        cr:translate((width - config.overall_width * scale) / 2, 0)
    end
    local pl = lgi.Pango.Layout.create(cr)
    for k, v in pairs(config.outputs) do
        cr:set_source(gcolor(beautiful.fg_normal))
        cr:rectangle(v.x * scale, v.y * scale, v.width * scale, v.height * scale)
        cr:fill()
        local lw, lh
        local current_font_size = dpi(beautiful.font_size_normal)
        local label = k
        if v.rotate then
            label = label.."\n"..rotate_label[v.rotate]
        end
        for i = 1, 2 do
            pl:set_font_description(beautiful.get_merged_font(beautiful.font, current_font_size))
            pl:set_text(label)
            pl:set_alignment("CENTER")
            lw, lh = pl:get_size()
            lw = lw / lgi.Pango.SCALE
            lh = lh / lgi.Pango.SCALE
            if lw <= v.width * scale and lh <= v.height * scale then break end
            current_font_size = current_font_size * math.min(v.width * scale / lw, v.height * scale / lh)
        end
        cr:move_to(v.x * scale + (v.width * scale - lw) / 2, v.y * scale + (v.height * scale - lh) / 2)
        cr:set_source(gcolor(beautiful.fg_focus))
        cr:show_layout(pl)
    end
end

local screen_layout_preview_max_size = dpi(120)
local function get_screen_layouts()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if e == nil then return false end
                if f == nil then return 0 end
                f = f:lower()
                local n = e.info.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local w = wibox.widget{
                    {
                        {
                            {
                                e.config and {
                                    forced_width = screen_layout_preview_max_size,
                                    forced_height = math.min(
                                        screen_layout_preview_max_size,
                                        math.ceil(e.config.overall_height / e.config.overall_width *
                                                  screen_layout_preview_max_size)),
                                    bgimage = function (context, cr, width, height)
                                        draw_autorandr_config(e.config, cr, width, height)
                                    end,
                                    widget = wibox.container.background,
                                },
                                {
                                    {
                                        text = e.info.name..
                                            (e.info.detected and " (detected)" or "")..
                                            (e.info.current and " (current)" or ""),
                                        widget = wibox.widget.textbox,
                                    },
                                    left = dpi(2),
                                    widget = wibox.container.margin,
                                },
                                layout = fixed_align.horizontal,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    local cmd
                    cmd = {"autorandr", e.info.name}
                    waffle:hide()
                    awful.spawn(cmd, false)
                end
                function w:set_focused(v)
                    -- local preview = self:get_children_by_id("preview")[1]
                    if preview then preview.visible = v end
                    self.context_transformation = {highlighted = v}
                end
                e.widget = w
                return w
            end,
        },
    }
    local home_dir = os.getenv("HOME")
    awful.spawn.easy_async(
        "autorandr",
        function (stdout, _stderr, _exitreason, _exitcode)
            local configs = {}
            for line in string.gmatch(stdout, "[^\n\r]+") do
                local name, attr = line:match("^([^%s]+)%s*(.-)$")
                table.insert(configs, {name = name, detected = attr:find("(detected)") ~= nil, current = attr:find("(current)") ~= nil})
            end
            table.sort(
                configs, function (a, b)
                    if a.detected ~= b.detected then return a.detected end
                    return a.name < b.name
                end)
            local index = 0
            for _, info in ipairs(configs) do
                index = index + 1
                local this_index = index
                local this_info = info
                local file = gio.File.new_for_path(home_dir.."/.config/autorandr/"..info.name.."/config")
                file:load_contents_async(
                    nil,
                    function (_source, result)
                        local contents, _error = file:load_contents_finish(result)
                        local config
                        if contents then
                            config = get_autorandr_config(contents)
                        end
                        output.array[this_index] = {info = this_info, config = config}
                        output:emit_signal("property::children")
                    end)
            end
            output:set_size(index)
        end
    )
    return ret
end

local previous_zshcompserver_pid
local function get_zsh_completion(bento)
    local input_stack = {}
    local start_array = {
        { name = "Kill process", value = "kill" },
        { name = "xinput manipulation", value = "xinput" },
    }
    local output = source.array_proxy()
    if previous_zshcompserver_pid ~= nil then
        print("Killing previous zshcompserver "..previous_zshcompserver_pid)
        awful.spawn({"kill", previous_zshcompserver_pid}, false)
    end
    local pid_or_error, _, stdin_fd, stdout_fd, stderr_fd = capi.awesome.spawn({"zshcompserver"},
        --[[use_sn=]]false, --[[use_stdin=]]true, --[[use_stdout=]]true, --[[use_stderr=]]true,
        --[[exit_callback=]]function (_exit_type)
        end, --[[env=]]nil)
    if type(pid_or_error) == "string" then
        print("Failed to launch zshcompserver: "..pid_or_error)
        return nil
    end
    previous_zshcompserver_pid = pid_or_error
    local stdin = gio.UnixOutputStream.new(stdin_fd, --[[close_fd=]]true)
    local stdout = gio.UnixInputStream.new(stdout_fd, --[[close_fd=]]true)
    local current_item = {}
    local reading_done = true
    local SPECIAL_CURRENT_INPUT = 0
    local SPECIAL_PREVIOUS_INPUT = 1
    awful.spawn.read_lines(
        stdout, function (line)
            if line == "" then
                output:set_size(#output.array)
                reading_done = true
                return
            end
            if current_item.value == nil then
                current_item.value = line
            else
                current_item.name = line
                output.array[#output.array + 1] = current_item
                current_item = {}
            end
        end
    )
    local function last_input()
        return #input_stack == 0 and "" or input_stack[#input_stack]
    end
    local function send_input()
        if not reading_done then
            print("Error! Attempting to send input while previous reading is in progress")
            return
        end
        if #input_stack > 0 then
            reading_done = false
            local input = input_stack[#input_stack]
            output.array = {}
            output.array[1] = { name = "RUN: "..input, special = SPECIAL_CURRENT_INPUT }
            output.array[2] = { name = (#input_stack > 1 and "BACK: "..input_stack[#input_stack - 1] or "BACK TO EMPTY"), special = SPECIAL_PREVIOUS_INPUT }
            input = last_input()
            output:set_size(#output.array)
            current_item = {}
            stdin:write_all(input.."\n")
            print("Sent input ["..input.."]")
        else
            output.array = start_array
            output:set_size(#output.array)
        end
    end
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if e == nil then return false end
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local w = wibox.widget{
                    {
                        {
                            {
                                e.config and {
                                    forced_width = screen_layout_preview_max_size,
                                    forced_height = math.min(
                                        screen_layout_preview_max_size,
                                        math.ceil(e.config.overall_height / e.config.overall_width *
                                                  screen_layout_preview_max_size)),
                                    bgimage = function (context, cr, width, height)
                                        draw_autorandr_config(e.config, cr, width, height)
                                    end,
                                    widget = wibox.container.background,
                                },
                                {
                                    {
                                        text = e.name,
                                        widget = wibox.widget.textbox,
                                    },
                                    left = dpi(2),
                                    widget = wibox.container.margin,
                                },
                                layout = fixed_align.horizontal,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    if e.special == SPECIAL_CURRENT_INPUT then
                        waffle:hide()
                        awful.spawn.with_shell(last_input(), false)
                        return
                    elseif e.special == SPECIAL_PREVIOUS_INPUT then
                        input_stack[#input_stack] = nil
                        send_input()
                        bento:reset_input()
                        return
                    end
                    input_stack[#input_stack + 1] = last_input()..e.value.." "
                    send_input()
                    bento:reset_input()
                end
                function w:set_focused(v)
                    -- local preview = self:get_children_by_id("preview")[1]
                    if preview then preview.visible = v end
                    self.context_transformation = {highlighted = v}
                end
                e.widget = w
                return w
            end,
        },
    }
    send_input()
    return ret
end

local calendar = source.concat{
    upstreams = {
        source.filterable{
            upstream = notix.pinned_container,
            callbacks = {
                filter = function (input, e)
                    for _, t in ipairs{e.notif.app_name, e.notif.title, e.notif.message} do
                        local succ, start = pcall(string.find, t:lower(), input or "")
                        if succ and start then return start end
                    end
                    return false
                end,
                reduce = function (filtered_results)
                    table.sort(filtered_results, function (a, b)
                                   if a[2] == b[2] then return a[1] < b[1] end
                                   return a[2] < b[2]
                               end)
                end,
            },
        },
        source.filterable{
            upstream = notix.regular_container,
            callbacks = {
                filter = function (input, e)
                    for _, t in ipairs{e.notif.app_name, e.notif.title, e.notif.message} do
                        local succ, start = pcall(string.find, t:lower(), input or "")
                        if succ and start then return start end
                    end
                    return false
                end,
                reduce = function (filtered_results)
                    table.sort(filtered_results, function (a, b)
                                   if a[2] == b[2] then return a[1] < b[1] end
                                   return a[2] < b[2]
                               end)
                end,
            },
        },
        source.filterable{
            upstream = cwidget.orgenda_items_widget,
            callbacks = {
                filter = function (input, e)
                    local succ, start = pcall(string.find, e.item.text:lower(), input or "")
                    return succ and start
                end,
                reduce = function (filtered_results)
                    table.sort(filtered_results, function (a, b)
                                   if a[2] == b[2] then return a[1] < b[1] end
                                   return a[2] < b[2]
                               end)
                end,
            },
        }
    },
}

local function get_tray_items()
    local output = source.array_proxy()
    if capi.awesome.systray_list then
        for index, info in ipairs(capi.awesome.systray_list()) do
            if xid and name then
                output:append{
                    xid = info[1],
                    name = info[2],
                }
            end
        end
    end
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            reduce = function (filtered_results)
                table.sort(filtered_results, function (a, b)
                               if a[2] == b[2] then return a[1] < b[1] end
                               return a[2] < b[2]
                           end)
            end,
            post_filter = function (e)
                if e.widget then return e.widget end
                local w = wibox.widget{
                    {
                        {
                            {
                                text = e.name,
                                widget = wibox.widget.textbox,
                            },
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        draw_pickers = {
                            fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
                            prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
                        },
                        widget = prism.container.background,
                    },
                    widget = prism.layer,
                }
                function w:execute()
                    awful.spawn({'activate-tray-window', tostring(e.xid), ''}, false)
                    waffle:hide()
                end
                function w:set_focused(v)
                    self.context_transformation = {highlighted = v}
                end
                e.widget = w
                return w
            end,
        },
    }
    return ret
end

return {
    get_apps = get_apps,
    get_audio_sinks = get_audio_sinks,
    get_audio_sources = get_audio_sources,
    get_screen_layouts = get_screen_layouts,
    get_zsh_completion = get_zsh_completion,
    calendar = calendar,
    get_tray_items = get_tray_items,
}

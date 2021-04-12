-- orgenda: a Awesome WM module of org-mode integration.
--
-- Author: Xinhao Yuan <xinhaoyuan@gmail.com>
-- License: MIT license, see LICENSE file.

local capi = {
    awesome = awesome,
}
local awful = require("awful")
local gobject = require("gears.object")
local gtimer = require("gears.timer")
local gdebug = require("gears.debug")
local gstring = require("gears.string")
local naughty = require("naughty")
local orgenda = {
    config = {
        files = {}
    },
    data = gobject{
        class = {},
        enable_properties = true,
        enable_auto_signals = true
    },
}

local function parse_timestamp(timestamp)
    local year, month, day, weekday, opt_rest = timestamp:match("^([0-9][0-9][0-9][0-9])%-([0-9][0-9])%-([0-9][0-9]) ([A-Za-z]+)(.*)$")
    local hour, minute
    if opt_rest and #opt_rest > 5 then
        hour, minute = opt_rest:match("^ ([0-9][0-9]):([0-9][0-9])")
    end
    return os.time{
        year = tonumber(year), month = tonumber(month), day = tonumber(day),
        hour = tonumber(hour), min = tonumber(minute)
    }, hour ~= nil
end

local function parse_todo_match(todo_match, decorator)
    local ret = {}
    local _, priority_end, priority  = todo_match:find("^%[#([A-C])%]%s+")
    ret.implicit_priority = priority == nil
    ret.priority = ret.implicit_priority and 2 or 3 + string.byte("A") - string.byte(priority)
    local text_begin = priority_end == nil and 1 or priority_end + 1
    local tag_begin, tag_end = todo_match:find("%s+[A-Z]*:.*")
    local tag_length = tag_begin == nil and 0 or tag_end - tag_begin + 1
    local text = tag_begin == nil and
        todo_match:sub(text_begin) or
        todo_match:sub(text_begin, -tag_length)
    ret.text = decorator(text)
    return ret
end

local function parse_attributes(line, todo_item)
    local match_begin = 1
    while true do
        local tag_begin, tag_end, name, timestamp = line:find("([A-Z]+):%s*<([^>]+)>", match_begin)
        if tag_begin == nil then break end
        match_begin = tag_end + 1

        if name == "SCHEDULED" or name == "DEADLINE" then
            todo_item.timestamp, todo_item.has_time = parse_timestamp(timestamp)
        end
    end
end

function orgenda.compare_todo_items(a, b)
    if a.priority ~= b.priority then
        return a.priority > b.priority
    end

    if a.timestamp ~= b.timestamp then
        if a.timestamp == nil then
            return false
        elseif b.timestamp == nil then
            return true
        else
            return a.timestamp < b.timestamp
        end
    end

    if a.rank ~= b.rank then
        return a.rank < b.rank
    end

    if a.source ~= b.source then
        return a.source < b.source
    end

    return a.line_number < b.line_number
end

local function parse_file(file_info, items)
    local path, decorator, rank
    if type(file_info) ~= "table" then
        file_info = { path = file_info }
    end
    path = file_info.path
    decorator = file_info.decorator or gstring.xml_escape
    rank = file_info.rank or 0
    local fd = io.open(path, "r")
    if not fd then
        gdebug.print_error("cannot open file at "..tostring(path))
        return {}
    end

    local todo_item = nil
    local line_number = 0
    for line in fd:lines() do
        line_number = line_number + 1
        if line:find("^%s*#") == nil then
            local header_text = line:match("^%s*[*]+%s*(.+)$")
            if header_text == nil then
                if todo_item ~= nil then
                    parse_attributes(line, todo_item)
                end
            else
                if todo_item ~= nil then
                    table.insert(items, todo_item)
                end
                local todo_type, todo_text = header_text:match("^(%u+)%s+(.*)$")
                if todo_type == "TODO" or todo_type == "DONE" then
                    todo_item = parse_todo_match(todo_text, decorator)
                    todo_item.done = todo_type == "DONE"
                    todo_item.source = path
                    todo_item.line_number = line_number
                    todo_item.rank = rank
                else
                    todo_item = nil
                end
            end
        end
    end
    if todo_item ~= nil then
        table.insert(items, todo_item)
    end
end

-- Merge multiple reset requests to improve performance.
orgenda._reset_timer = gtimer {
    timeout = 0.1,
    single_shot = true,
    callback = function ()
        orgenda._reset_scheduled = false
        orgenda.reset()
    end,
}
function orgenda.schedule_reset()
    if orgenda._reset_scheduled then
        return
    end
    orgenda._reset_timer:again()
    orgenda._reset_scheduled = true
end

function orgenda.reset()
    local items = {}
    for _, file_info in pairs(orgenda.config.files) do
        parse_file(file_info, items)
    end
    orgenda.data.items = items
    naughty.notify{
        skip_notix = true,
        title = "Orgenda reloaded."
    }
end

function orgenda.hide(item)
    local cmd = { "sed", "-e", tostring(item.line_number).."s/ TODO\\| DONE//", "-i", item.source }
    awful.spawn.easy_async(cmd, orgenda.schedule_reset)
end

function orgenda.toggle_done(item)
    item.done = not item.done
    orgenda.data:emit_signal("property::items")
    orgenda.save_priority_status(item)
end

function orgenda.promote(item)
    item.priority = item.priority % 3 + 1
    orgenda.data:emit_signal("property::items")
    orgenda.save_priority_status(item)
end

function orgenda.save_priority_status(item)
    local pri_char = ({"C", "B", "A"})[item.priority]
    local cmd = { "sed", "-e", tostring(item.line_number).."s/ \\(TODO\\|DONE\\) *\\(\\[#[A-C]\\]\\|\\) / "..
                      (item.done and "DONE" or "TODO")..
                      (item.priority == 2 and item.implicit_priority and "" or
                       " [#"..pri_char.."]")..
                      " /", "-i", item.source }
    awful.spawn.easy_async(cmd, function () end)
end

function orgenda.widget(args)
    args = args or {}

    local orgenda_widget

    local wibox = require("wibox")
    local fixed_margin = require("fixed_margin")
    local beautiful = require("beautiful")
    local cbg = require("contextual_background")

    local todo_item_container = wibox.widget {
        widget = wibox.layout.fixed.vertical
    }
    orgenda_widget = todo_item_container

    local function get_mark(item)
        return beautiful["orgenda_mark_p"..tostring(item.priority).."_"..(item.done and "done" or "todo")]
    end

    local todo_item_widget_cache = {}

    local function get_todo_item_widget(item, cache_key)
        if todo_item_widget_cache[cache_key] == nil then
            local widget = wibox.widget{
                {
                    {
                        {
                            {
                                image = get_mark(item),
                                forced_width = beautiful.icon_size,
                                forced_height = beautiful.icon_size,
                                widget = wibox.widget.imagebox,
                            },
                            valign = "top",
                            widget = wibox.container.place
                        },
                        right = beautiful.sep_small_size,
                        widget = wibox.container.margin,
                    },
                    {
                        {
                            item.timestamp and {
                                id = "timestamp",
                                -- text will be updated by the following function.
                                font = args.font,
                                widget = wibox.widget.textbox
                                               },
                            {
                                {
                                    markup = item.text,
                                    font = args.font,
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
                fg_function = {"fg_"},
                bg_function = {"bg_"},
                context_transform_function = {focus = false},
                widget = cbg,
            }
            widget:connect_signal(
                "mouse::enter",
                function (w)
                    w:set_context_transform_function{ focus = true }
                end
            )
            widget:connect_signal(
                "mouse::leave",
                function (w)
                    w:set_context_transform_function{ focus = false }
                end
            )
            widget:connect_signal(
                "button::release",
                function (w, _x, _y, button)
                    if button == 1 then
                        orgenda.toggle_done(w.item)
                    elseif button == 2 then
                        orgenda.hide(w.item)
                    elseif button == 3 then
                        orgenda.promote(w.item)
                    end
                end
            )
            todo_item_widget_cache[cache_key] = widget
        end
        todo_item_widget_cache[cache_key].item = item
        return todo_item_widget_cache[cache_key]
    end

    local function scan_for_expired_items()
        local time = os.time()
        local date = os.date("%Y%m%d", time)
        for _, widget in pairs(todo_item_widget_cache) do
            local item = widget.item
            local expired = false
            if item.has_time and item.timestamp <= time then
                expired = true
            elseif item.timestamp ~= nil then
                local item_date = os.date("%Y%m%d", item.timestamp)
                expired = item_date < date
            end
            if expired then
                widget:get_children_by_id("timestamp")[1].markup = '<span strikethrough="true"><b>['..os.date(item.has_time and "%Y-%m-%d %a %H:%M" or "%Y-%m-%d %a", item.timestamp)..']</b></span>'
            elseif item.timestamp ~= nil then
                widget:get_children_by_id("timestamp")[1].markup = '<b>['..os.date(item.has_time and "%Y-%m-%d %a %H:%M" or "%Y-%m-%d %a", item.timestamp)..']</b>'
            end
        end
    end

    local function get_cache_key(item)
        return tostring(item.timestamp)..':'..(item.done and "!" or "?")..tostring(item.priority)..':'..item.text
    end

    orgenda.data:connect_signal(
        "property::items",
        function ()
            table.sort(orgenda.data.items, orgenda.compare_todo_items)
            todo_item_container:reset()
            local cache_keys = {}
            for index, item in ipairs(orgenda.data.items) do
                local cache_key = get_cache_key(item)
                cache_keys[cache_key] = true
                todo_item_container:add(get_todo_item_widget(item, cache_key))
            end
            for k, v in pairs(todo_item_widget_cache) do
                if not cache_keys[k] then
                    todo_item_widget_cache[k] = nil
                end
            end
            scan_for_expired_items()
        end
    )

    gtimer {
        timeout = 300,
        autostart = true,
        callback = scan_for_expired_items,
    }

    return orgenda_widget
end

capi.awesome.connect_signal("orgenda::request_reset", orgenda.schedule_reset)

gtimer.delayed_call(
    function ()
        if #orgenda.config.files == 0 then
            gdebug.print_warning("orgenda: no files to watch - will do nothing.")
            return
        end
        local cmd = {"fswatch", "-x", "--event=Updated", "--event=AttributeModified"}
        for _, file_info in ipairs(orgenda.config.files) do
            local path = type(file_info) == "table" and file_info.path or file_info
            table.insert(cmd, path)
        end
        awful.spawn.with_line_callback(
            cmd,
            {
                stdout = function(line)
                    -- gdebug.print_warning("Got fswatch line: "..line)
                    orgenda.schedule_reset()
                end
            }
        )
        orgenda.reset()
    end
)

return orgenda

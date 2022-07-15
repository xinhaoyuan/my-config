local capi = {
    awesome = awesome,
}
local awful = require("awful")
local gobject = require("gears.object")
local gtimer = require("gears.timer")
local gdebug = require("gears.debug")
local gstring = require("gears.string")
local naughty = require("naughty")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi
local beautiful = require("beautiful")
local glib = require("lgi").GLib
local orgenda = {
    config = {
        files = {
            -- Entry can be a table, e.g.
            -- {
            --   path = [SOME PATH STRING],
            --   rank = [SOME NUMBER FOR ORDERING ITEMS IN TIES],
            -- }
            -- or simply a path, where the rank will be 0.
        },
        watch_files = true,
        show_notifications = false,
        reset_merge_threshold_sec = 0.1,
        reset_max_delay_sec = 0.5,
        expiration_scan_interval_sec = 300,
        -- Default values for keys below will be set up later in the module.
        compare_items_cb = nil,
        widget_item_template = nil,
    },
    data = gobject{
        class = {},
        enable_properties = true,
        enable_auto_signals = true,
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
    local tags_begin, tags_end, tags_text = todo_match:find("%s*:([^%s<>]+):%s*$")
    local tags_length = tags_begin == nil and 0 or tags_end - tags_begin + 1
    local text = tags_begin == nil and
        todo_match:sub(text_begin) or
        todo_match:sub(text_begin, -1 - tags_length)
    ret.text = text
    ret.decorated_text = decorator(text)
    if tags_text ~= nil then
        local tags = {}
        for tag in string.gmatch(tags_text, "[^:]+") do
            tags[tag] = true
        end
        ret.tags_text = tags_text
        ret.tags = tags
    end
    return ret
end

local function parse_timestamps(line, todo_item)
    local match_begin = 1
    while true do
        local name_begin, name_end, name, timestamp = line:find("([A-Z]+):%s*<([^>]+)>", match_begin)
        if name_begin == nil then break end
        match_begin = name_end + 1

        if name == "SCHEDULED" or name == "DEADLINE" then
            todo_item.timestamp, todo_item.has_time = parse_timestamp(timestamp)
        end
    end
end

function orgenda.config.compare_items_cb(a, b)
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
                    parse_timestamps(line, todo_item)
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

local reset_timer
local reset_reschedule_deadline
function orgenda.schedule_reset()
    local now = glib.get_monotonic_time()
    if reset_timer then
        if now < reset_reschedule_deadline then
            reset_timer:again()
        end
        return
    end
    reset_reschedule_deadline = now
        + (orgenda.config.reset_max_delay_sec
           - orgenda.config.reset_merge_threshold_sec)
        * 1000000
    reset_timer = gtimer{
        timeout = orgenda.config.reset_merge_threshold_sec,
        single_shot = true,
        autostart = true,
        callback = function ()
            reset_timer = nil
            orgenda.reset()
        end,
    }
end

function orgenda.reset()
    local items = {}
    for _, file_info in pairs(orgenda.config.files) do
        parse_file(file_info, items)
    end
    orgenda.data.items = items
    if orgenda.config.show_notifications then
        naughty.notify{
            title = "Orgenda reloaded.",
            timeout = 3,
        }
    end
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
    awful.spawn.easy_async(cmd, orgenda.schedule_reset)
end

orgenda.config.widget_item_template = {
    {
        {
            {
                {
                    id = "status_role",
                    align = "center",
                    widget = wibox.widget.textbox,
                },
                width = dpi(40),
                strategy = "exact",
                widget = wibox.container.constraint,
            },
            valign = "top",
            widget = wibox.container.place,
        },
        right = dpi(5),
        widget = wibox.container.margin,
    },
    {
        {
            {
                id = "timestamp_role",
                ellipsize = "none",
                align = "left",
                valign = "center",
                wrap = "word_char",
                widget = wibox.widget.textbox,
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
            layout = wibox.layout.fixed.vertical,
        },
        valign = "center",
        widget = wibox.container.place,
    },
    layout = wibox.layout.fixed.horizontal,
}

function get_status(item)
    if item.done then return "[X]" end
    return "["..(item.implicit_priority and " " or tostring(item.priority)).."]"
end

function orgenda.widget(args)
    args = args or {}
    local handle_expired_items = args.handle_expired_items
    if handle_expired_items == nil then handle_expired_items = true end
    local item_by_widget = setmetatable({}, {__mode="k"})

    local create_item_widget = args.create_item_widget_cb or function ()
        local widget = wibox.widget.base.make_widget_from_value(orgenda.config.widget_item_template)
        widget:connect_signal(
            "button::release",
            function (w, _x, _y, button)
                if button == 1 then
                    orgenda.toggle_done(item_by_widget[w])
                elseif button == 2 then
                    orgenda.hide(item_by_widget[w])
                elseif button == 3 then
                    orgenda.promote(item_by_widget[w])
                end
            end
        )
        return widget
    end
    local update_item_widget = args.update_item_widget_cb or function (widget, item)
        local status = widget:get_children_by_id("status_role")
        if #status > 0 then status[1].text = get_status(item)  end
        local text = widget:get_children_by_id("text_role")
        if #text > 0 then text[1].markup = item.decorated_text end
    end
    local orgenda_widget

    local todo_item_container = wibox.widget {
        layout = args.layout or wibox.layout.fixed.vertical,
    }
    orgenda_widget = todo_item_container

    local widget_by_item_key = {}

    local function get_todo_item_widget(item, item_key) --
        local widget = widget_by_item_key[item_key]
        if widget_by_item_key[item_key] == nil then
            widget = create_item_widget(item)
            widget_by_item_key[item_key] = widget
        end
        if widget then
            update_item_widget(widget, item)
            item_by_widget[widget] = item
        end
        return widget
    end

    local function scan_for_expired_items()
        if not handle_expired_items then return end
        local time = os.time()
        local date = os.date("%Y%m%d", time)
        for _, widget in pairs(widget_by_item_key) do
            local item = item_by_widget[widget]
            local expired = false
            local timestamp_children = widget:get_children_by_id("timestamp_role")
            if #timestamp_children > 0 then
                if item.has_time and item.timestamp <= time then
                    expired = true
                elseif item.timestamp ~= nil then
                    local item_date = os.date("%Y%m%d", item.timestamp)
                    expired = item_date < date
                end
                if expired then
                    timestamp_children[1].markup = "<span strikethrough='true'><b>["..os.date(item.has_time and "%Y-%m-%d %a %H:%M" or "%Y-%m-%d %a", item.timestamp).."]</b></span>"
                elseif item.timestamp ~= nil then
                    timestamp_children[1].markup = "<b>["..os.date(item.has_time and "%Y-%m-%d %a %H:%M" or "%Y-%m-%d %a", item.timestamp).."]</b>"
                end
            end
        end
    end

    local function get_item_key(item)
        -- return tostring(item.timestamp)..":"..(item.done and "!" or "?")..tostring(item.priority)..":"..item.decorated_text..":"..(item.tags_text or "")
        return item.line_number
    end

    orgenda.data:connect_signal(
        "property::items",
        function ()
            local items = {}
            for i = 1, #orgenda.data.items do items[i] = orgenda.data.items[i] end
            table.sort(items, orgenda.config.compare_items_cb)
            todo_item_container:reset()
            local item_keys = {}
            for index, item in ipairs(items) do
                local item_key = get_item_key(item)
                item_keys[item_key] = true
                local widget = get_todo_item_widget(item, item_key)
                if widget then
                    todo_item_container:add(widget)
                end
            end
            for k, v in pairs(widget_by_item_key) do
                if not item_keys[k] then
                    widget_by_item_key[k] = nil
                end
            end
            scan_for_expired_items()
            todo_item_container:emit_signal("property::children")
        end
    )

    if handle_expired_items then
        gtimer {
            timeout = orgenda.config.expiration_scan_interval_sec,
            autostart = true,
            callback = scan_for_expired_items,
        }
    end

    return orgenda_widget
end

local init_flag = false
function orgenda.init(config)
    if init_flag then return orgenda else init_flag = true end

    if config then
        assert(type(config) == "table")
        for k, v in pairs(config) do
            orgenda.config[k] = v
        end
    end

    if orgenda.config.watch_files then
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
    end

    return orgenda
end

return orgenda

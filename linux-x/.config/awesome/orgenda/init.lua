local awful = require("awful")
local gobject = require("gears.object")
local hotpot = require("hotpot")

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

local function parse_todo_match(todo_match)
    local _, priority_end, priority  = todo_match:find("^%[#([A-C])%]%s+")
    priority = priority == nil and 2 or 3 + string.byte("A") - string.byte(priority)
    local text_begin = priority_end == nil and 1 or priority_end + 1
    local tag_begin, tag_end = todo_match:find("%s+[A-Z]*:.*")
    local tag_length = tag_begin == nil and 0 or tag_end - tag_begin + 1
    if tag_begin == nil then
        return { priority = priority, text = todo_match:sub(text_begin) }
    else
        return { priority = priority, text = todo_match:sub(text_begin, -tag_length) }
    end
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
    if a.priority > b.priority then
        return true
    elseif a.priority < b.priority then
        return false
    end

    if a.timestamp == nil then
        return false
    elseif b.timestamp == nil then
        return true
    else
        return a.timestamp < b.timestamp
    end
end

local function parse_file(path, items)
    local fd = io.open(path, "r")
    if not fd then
        hotpot.logging.error("cannot open file at "..tostring(path))
        return {}
    end

    local todo_item = nil
    for line in fd:lines() do
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
                if todo_type == "TODO" then
                    todo_item = parse_todo_match(todo_text)
                    todo_item.source = path
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

function orgenda.reset()
    local items = {}
    for _, path in pairs(orgenda.config.files) do
        parse_file(path, items)
    end
    table.sort(items, orgenda.compare_todo_items)
    orgenda.data.items = items
end

orgenda.widget = {}
function orgenda.widget.create(args)
    args = args or {}

    local orgenda_widget

    local wibox = require("wibox")
    local fixed_margin = require("fixed_margin")
    local beautiful = require("beautiful")

    local todo_item_container = wibox.widget {
        widget = wibox.layout.fixed.vertical
    }
    orgenda_widget = wibox.widget {
        {
            todo_item_container,
            margins = args.item_margin,
            draw_empty = false,
            widget = fixed_margin,
        },
        width = args.width,
        widget = wibox.container.constraint
    }

    -- TODO: This does not include the indent width.
    local item_width = args.width and args.item_margin and args.indent_width and
        args.width - args.item_margin * 2 - args.indent_width

    local function render_priority(pri)
        return ({ " +", " *","<span foreground='"..beautiful.fg_urgent.."' background='"..beautiful.bg_urgent.."'><b>!!</b></span>" })[pri]
    end

    local todo_item_widget_cache = {}

    local function get_todo_item_widget(item, cache_key)
        if todo_item_widget_cache[cache_key] == nil then
            todo_item_widget_cache[cache_key] = wibox.widget {
                {
                    {
                        markup = render_priority(item.priority)..' ',
                        font = args.font,
                        widget = wibox.widget.textbox,
                        forced_width = args.indent_width,
                    },
                    valign = "top",
                    widget = wibox.container.place
                },
                {
                    item.timestamp and {
                        markup = '<b>['..os.date(item.has_time and "%Y-%m-%d %a %H:%M" or "%Y-%m-%d %a", item.timestamp)..']</b>',
                        font = args.font,
                        widget = wibox.widget.textbox
                                  },
                    {
                        text = item.text,
                        font = args.font,
                        ellipsize = "none",
                        align = "left",
                        wrap = "word_char",
                        forced_width = item_width, -- Needed to calculate the extents properly.
                        widget = wibox.widget.textbox
                    },
                    widget = wibox.layout.fixed.vertical
                },
                widget = wibox.layout.fixed.horizontal
            }
        end
        return todo_item_widget_cache[cache_key]
    end

    orgenda.data:connect_signal(
        "property::items",
        function ()
            todo_item_container:reset()
            local cache_keys = {}
            for index, item in ipairs(orgenda.data.items) do
                local cache_key = tostring(item.timestamp)..':'..tostring(item.priority)..':'..item.text
                cache_keys[cache_key] = true
                todo_item_container:add(get_todo_item_widget(item, cache_key))
            end
            for k, v in pairs(todo_item_widget_cache) do
                if not cache_keys[k] then
                    todo_item_widget_cache[k] = nil
                end
            end
            -- local count = 0
            -- for _, _ in pairs(todo_item_widget_cache) do
            --     count = count + 1
            -- end
            -- print(tostring(count)..' items remaining in the cache')
        end
    )

    return orgenda_widget
end


hotpot.on_ready(
    function ()
        if #orgenda.config.files == 0 then
            hotpot.logging.warning("orgenda: no files to watch - will do nothing.")
            return
        end
        local cmd = {"fswatch", "-x", "--event=Updated"}
        for _, file in ipairs(orgenda.config.files) do
            table.insert(cmd, file)
        end
        awful.spawn.with_line_callback(
            cmd,
            {
                stdout = function(line)
                    hotpot.logging.info("Got fswatch line: ", line)
                    orgenda.reset()
                end
            }
        )
        orgenda.reset()
    end
)

return orgenda

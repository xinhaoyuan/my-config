local awful = require("awful")
local gobject = require("gears.object")
local hotpot = require("hotpot")

local mod = {
    config = {
        files = {}
    },
    topic = gobject{class = {}},
}

local function parse_timestamp(timestamp)
    return timestamp:match("[-0-9]+")
end

local function parse_todo_match(todo_match)
    local _, priority_end, priority  = todo_match:find("^%[#([A-C])%]%s+")
    priority = priority == nil and 1 or 3 +  string.byte("A") - string.byte(priority)
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
            todo_item.date = parse_timestamp(timestamp)
        end
    end
end

local function compare_todo_item(a, b)
    if a.priority > b.priority then
        return true
    elseif a.priority < b.priority then
        return false
    end

    if a.date == nil then
        return false
    elseif b.date == nil then
        return true
    else
        return a.date < b.date
    end
end

local function parse_file(path)
    local fd = io.open(path, "r")
    if not fd then
        hotpot.logging.error("cannot open file at "..tostring(path))
        return {}
    end

    local results = {}
    local todo_item = nil
    for line in fd:lines() do
        if line:find("^%s*#") == nil then
            local todo_match = line:match("TODO%s+(.+)")
            if todo_match ~= nil then
                if todo_item ~= nil then
                    table.insert(results, todo_item)
                end
                todo_item = parse_todo_match(todo_match)
            end

            if todo_item ~= nil then
                parse_attributes(line, todo_item)
            end
        end
    end
    if todo_item ~= nil then
        table.insert(results, todo_item)
    end
    table.sort(results, compare_todo_item)
    return results
end

function mod.reset()
    for _, path in ipairs(mod.config.files) do
        hotpot.logging.info("parsing ", path)
        local data = parse_file(path)
        mod.topic:emit_signal("update", path, data)
    end
end

hotpot.config.on_ready(
    function ()
        if #mod.config.files == 0 then
            hotpot.logging.warning("orgenda: no files to watch - will do nothing.")
            return
        end
        local cmd = {"fswatch", "-x", "--event=Updated"}
        for _, file in ipairs(mod.config.files) do
            table.insert(cmd, file)
        end
        awful.spawn.with_line_callback(
            cmd,
            {
                stdout = function(line)
                    hotpot.logging.info("Got fswatch line: ", line)
                    mod.reset()
                end
            }
        )
        mod.reset()
    end
)

return mod

local awful = require("awful")
local gobject = require("gears.object")
local hotpot = require("hotpot")

local mod = {
    config = {
        files = {}
    },
    topic = gobject{class = {}},
}

local function parse_file(path)
    local fd = io.open(path, "r")
    if not fd then
        hotpot.logging.error("cannot open file at "..tostring(path))
        return {}
    end

    local results = {}
    for line in fd:lines() do
        local text = line:match(".-TODO%s+(.*)")
        if text ~= nil then
            table.insert(results, text)
        end
    end
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
            hotpot.logging.warning("organda: no files to watch - will do nothing.")
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

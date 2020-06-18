local hotpot = {
    config = {
        -- A fix to reduce memory leakage possibly due to process spawning.
        force_gc_timeout = 30,
    },
    focus_timestamp = require("hotpot.focus_timestamp"),
    logging = require("hotpot.logging"),
}

local gtimer = require("gears.timer")
function hotpot.on_ready(...)
    gtimer.delayed_call(...)
end

hotpot.on_ready(function ()
        if hotpot.config.force_gc_timeout ~= nil then
            gtimer {
                timeout = hotpot.config.force_gc_timeout,
                autostart = true,
                callback = function ()
                    collectgarbage("collect")
                end,
            }
        end
end)

return hotpot

local gtimer = require("gears.timer")
local mod = {}

function mod.on_ready(...)
    gtimer.delayed_call(...)
end

return mod

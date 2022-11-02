local awful = require("awful")

local orig_disable_tracking = awful.client.focus.history.disable_tracking
local orig_enable_tracking = awful.client.focus.history.enable_tracking

local enable_tracking_count = {}
local disable_tracking_count = {}

local function get_source()
    local result = ""
    for l = 3, 6 do
        local info = debug.getinfo(l, "Sl")
        if not info then break end
        if #result > 0 then result = result..";" end
        result = result..string.format("%s:%d", info.short_src, info.currentline)
    end
    return result
end

function awful.client.focus.history.disable_tracking(...)
    local src = get_source()
    disable_tracking_count[src] = (disable_tracking_count[src] or 0) + 1
    orig_disable_tracking(...)
end

function awful.client.focus.history.enable_tracking(...)
    local src = get_source()
    enable_tracking_count[src] = (enable_tracking_count[src] or 0) + 1
    orig_enable_tracking(...)
end

return function ()
    print("============================================================")
    print("Current tracking:", awful.client.focus.history.is_enabled()) 
    print("Enable count:")
    for k, v in pairs(enable_tracking_count) do
        print(k, v)
    end
    print("------------------------------------------------------------")
    print("Disable count:")
    for k, v in pairs(disable_tracking_count) do
        print(k, v)
    end
    print("============================================================")
end

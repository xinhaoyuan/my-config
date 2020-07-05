-- Maintains abstract timestamps of clients recording the time being focused.

local capi = {
    client = client
}
local awful = require("awful")

local mod = {}
local global_focus_timestamp = 0

local function try_compact_focus_timestamp()
    if global_focus_timestamp < capi.client.instances() * 10 then
        return
    end
    local clients = capi.client.get()
    table.sort(clients, function (a, b)
                   local a_timestamp = a.focus_timestamp or 0
                   local b_timestamp = b.focus_timestamp or 0
                   return a_timestamp < b_timestamp
    end)
    for i, c in ipairs(clients) do
        c.focus_timestamp = i
    end
    -- print("Compact focus timestamp from "..tostring(global_focus_timestamp).." to "..tostring(#clients))
    global_focus_timestamp = #clients
end

function mod.update(c)
    if c == nil then return end
    if c.focus_timestamp ~= nil and c.focus_timestamp > global_focus_timestamp
    then
        global_focus_timestamp = c.focus_timestamp
    end
    global_focus_timestamp = global_focus_timestamp + 1
    c.focus_timestamp = global_focus_timestamp
    try_compact_focus_timestamp()
end

function mod.get(c)
    return c.focus_timestamp or 0
end

capi.client.connect_signal(
    "focus",
    function (c)
        if awful.client.focus.history.is_enabled() then
            mod.update(c)
        end
    end
)

capi.client.connect_signal(
    "manage",
    function (c)
        if c.focus_timestamp == nil then
            c.focus_timestamp = 0
        else
            if c.focus_timestamp > global_focus_timestamp then
                global_focus_timestamp = c.focus_timestamp
                try_compact_focus_timestamp()
            end
        end
    end
)

awful.client.property.persist("focus_timestamp", "number")

return mod

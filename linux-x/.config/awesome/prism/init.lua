local function get_proxy(origin_table, transform_func)
    return setmetatable(
        {}, {
            __index = function(_self, key)
                local v = origin_table[key]
                if v == nil then return nil end
                return transform_func(v)
            end
        })
end

local wrap = require(... .. ".wrap")
return {
    picker = require(... .. ".picker"),
    layer = require(... .. ".layer"),
    wrap = wrap,
    container = get_proxy(require("wibox.container"), wrap),
    layout = get_proxy(require("wibox.layout"), wrap),
    widget = get_proxy(require("wibox.widget"), wrap),
}

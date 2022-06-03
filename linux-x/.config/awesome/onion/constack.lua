local wibox = require("wibox")

local module = {}

local constack_value_tomb = {}
local constack_last_layer = setmetatable({}, {__mode = "k"})
local constack_base_layer = setmetatable({}, {__mode = "k"})
local constack_from_layer = setmetatable({}, {__mode = "k"})

module.constack_mt = {
    __index = function(self, key)
        local head = constack_last_layer[self]
        local cur = head
        while cur ~= nil do
            local value = cur[key]
            if value ~= nil then
                -- An optimization that speeds up the read using storage.
                if cur ~= head then
                    head[key] = value
                end
                if value == constack_value_tomb then
                    return nil
                else
                    return value
                end
            end
            cur = constack_last_layer[cur]
        end
        return nil
    end,
    __newindex = function(self, key, value)
        local p = constack_last_layer[self]
        assert(p ~= constack_base_layer[self], "never modify the constack without pushing the base")
        assert(constack_last_layer[p] ~= nil)
        if value == nil then
            p[key] = constack_value_tomb
        else
            p[key] = value
        end
    end,
}

function module.get(c)
    if constack_base_layer[c] then
        return c
    end
    local ret = constack_from_layer[c]
    if ret == nil then
        ret = setmetatable({}, module.constack_mt)
        constack_base_layer[ret] = c
        constack_from_layer[c] = ret
        constack_last_layer[ret] = c
    end
    return ret
end

function module.push(constack)
    local ret = constack_last_layer[constack]
    local new_head = {}
    constack_last_layer[new_head] = ret
    constack_last_layer[constack] = new_head
    constack_from_layer[ret] = constack
    return ret
end

function module.pop(constack, to)
    if to == nil then
        to = constack_last_layer[constack_last_layer[constack]]
    end
    assert(type(to) == "table", "cannot pop the base from constack")
    assert(constack_from_layer[to] == constack, "cannot pop to layer not belonging to the constack")
    constack_last_layer[constack] = to
end

return module

local wibox = require("wibox")

print("loaded as", ...)
local module = {}

local contextack_value_tomb = {}
local contextack_parent = setmetatable({}, {__mode = "k"})
local contextack_base = setmetatable({}, {__mode = "k"})
local contextack_head = setmetatable({}, {__mode = "k"})

module.contextack_mt = {
    __index = function(self, key)
        local head = contextack_parent[self]
        local cur = head
        while cur ~= nil do
            local value = cur[key]
            if value ~= nil then
                -- An optimization that speeds up the read using storage.
                if cur ~= head then
                    head[key] = value
                end
                if value == contextack_value_tomb then
                    return nil
                else
                    return value
                end
            end
            cur = contextack_parent[cur]
        end
        return nil
    end,
    __newindex = function(self, key, value)
        local p = contextack_parent[self]
        assert(p ~= contextack_base[self], "never modify the contextack without pushing the base")
        assert(contextack_parent[p] ~= nil)
        if value == nil then
            p[key] = contextack_value_tomb
        else
            p[key] = value
        end
    end,
}

function module.get(c)
    if contextack_base[c] then
        return c
    end
    local ret = contextack_head[c]
    if ret == nil then
        ret = setmetatable({}, module.contextack_mt)
        contextack_base[ret] = c
        contextack_head[c] = ret
        contextack_parent[ret] = c
    end
    return ret
end

function module.push(contextack)
    local ret = contextack_parent[contextack]
    local new_head = {}
    contextack_parent[new_head] = ret
    contextack_parent[contextack] = new_head
    return ret
end

function module.pop(contextack, to)
    if to == nil then
        to = contextack_parent[contextack_parent[contextack]]
    end
    assert(type(to) == "table", "cannot pop the base from contextack")
    contextack_parent[contextack] = to
end

return module

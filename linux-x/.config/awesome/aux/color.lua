local mod = {}
local gcolor = require("gears.color")
local gtable = require("gears.table")

local class = { mt = {} }
class.mt.__index = class

function class:with_alpha(a)
    self[4] = a
    return self
end

function class:to_string(args)
    args = args or {}
    local ret = "#"
    local n_components = args.no_alpha and 3 or 4
    for i = 1, n_components do
        ret = ret .. string.format("%02x", math.min(255, math.floor(self[i] * 256)))
    end
    return ret
end

function class:blend_with(c, w)
    c = type(c) == "string" and mod.from_string(c) or c
    local ret = {}
    for i = 1, 4 do
        self[i] = self[i] * (1 - w) + c[i] * w
    end
    return self
end

function class:lightness()
    return (math.max(self[1], self[2], self[3]) + math.min(self[1], self[2], self[3])) / 2
end

function mod.from_string(s)
    local ret = {gcolor.parse_color(s)}
    setmetatable(ret, class.mt)
    return ret
end

setmetatable(mod, { __call = function (_, s) return mod.from_string(s) end })

return mod

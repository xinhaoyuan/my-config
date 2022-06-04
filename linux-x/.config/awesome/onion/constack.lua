local wibox = require("wibox")
local gcache = require("gears.cache")
local picker = require((...):match("(.-)[^%.]+$").."picker")
local module = {}
local setmetatable = setmetatable

local constack_value_tomb = {}
local constack_last_layer = setmetatable({}, {__mode = "k"})
local constack_layer_storage = setmetatable({}, {__mode = "k"})
local constack_layer_mt = {
    __index = function (self, key)
        local value = constack_layer_storage[self][key]
        if value == nil and constack_last_layer[self] then
            value = constack_last_layer[self][key]
            -- Make the access faster next time.
            if value == nil then
                value = constack_value_tomb
            end
            constack_layer_storage[self][key] = value
        end
        if value == constack_value_tomb then
            return nil
        else
            return value
        end
    end,
    __newindex = function (self, key, value)
        if value == nil then
            constack_layer_storage[self][key] = constack_value_tomb
        else
            constack_layer_storage[self][key] = value
        end
    end,
}
local constack_mt = {
    __index = function (self, key)
        return constack_last_layer[self][key]
    end,
    __newindex = function(self, key, value)
        local p = constack_last_layer[self]
        assert(constack_last_layer[p] ~= nil,
               "never modify the constack without pushing the base")
        p[key] = value
    end,
}
local function make_constack(top_layer)
    local ret = setmetatable({}, constack_mt)
    constack_last_layer[ret] = top_layer
    return ret
end

local function create_layer(last_layer)
    local new_layer = setmetatable({}, constack_layer_mt)
    constack_layer_storage[new_layer] = {}
    constack_last_layer[new_layer] = last_layer
    return new_layer
end

local constack_layer_cache = gcache.new(
    function(base_layer, transfromation_picker)
        assert(picker.is_picker(transfromation_picker))
        local modification = picker.eval_exhaustively(transfromation_picker, base_layer)
        assert(type(modification) == "table")
        local new_layer = create_layer(base_layer)
        for k, v in pairs(modification) do
            if picker.is_picker(v) then
                v = picker.eval_exhaustively(v, base)
            end
            new_layer[k] = v
        end
        return new_layer
    end)


local constack_base_layer = setmetatable({}, {__mode = "k"})
local constack_from_layer = setmetatable({}, {__mode = "k"})

function module.get(c)
    if constack_base_layer[c] then
        return c
    end
    local ret = constack_from_layer[c]
    if ret == nil then
        ret = make_constack(c)
        constack_base_layer[ret] = c
        constack_from_layer[c] = ret
    end
    return ret
end

function module.get_last_layer(c)
    return constack_last_layer[c]
end

function module.cached_push_and_transform(constack, transfromation_picker)
    local top_layer = constack_last_layer[constack]
    local new_top = constack_layer_cache:get(top_layer, transfromation_picker)
    constack_last_layer[constack] = new_top
    constack_from_layer[new_top] = constack
    return top_layer, new_top
end

function module.restore(constack, to_layer)
    assert(constack_from_layer[to_layer] == constack, "cannot pop to layer not belonging to the constack")
    constack_last_layer[constack] = to_layer
end

function module.push(constack)
    local ret = constack_last_layer[constack]
    local new_layer = create_layer(ret)
    constack_layer_storage[new_layer] = {}
    constack_last_layer[new_layer] = ret
    constack_last_layer[constack] = new_layer
    constack_from_layer[new_layer] = constack
    return ret, new_layer
end

function module.pop(constack)
    local to_layer = constack_last_layer[constack_last_layer[constack]]
    assert(type(to_layer) == "table", "cannot pop the base from constack")
    module.restore(constack, to_layer)
end

return module

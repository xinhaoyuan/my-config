local wibox = require("wibox")
local pcache = require((...):match("(.-)[^%.]+$").."pcache")
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

local data_constack_layer_cache = setmetatable({}, {__mode = "k"})
local function get_constack_layer_cache(data)
    local data_key = data == nil and constack_value_tomb or data
    local cache = data_constack_layer_cache[data_key]
    if cache == nil then
        cache = pcache.new(
            function(base_layer, transfromation_picker)
                assert(picker.is_picker(transfromation_picker))
                local modification = picker.eval_exhaustively(transfromation_picker, base_layer, data)
                assert(type(modification) == "table")
                local new_layer = create_layer(base_layer)
                for k, v in pairs(modification) do
                    if picker.is_picker(v) then
                        v = picker.eval_exhaustively(v, base_layer, data)
                    end
                    new_layer[k] = v
                end
                return new_layer
            end)
        data_constack_layer_cache[data_key] = cache
    end
    return cache
end
function module.clear_constack_layer_cache(data)
    data_constack_layer_cache[data == nil and constack_value_tomb or data] = nil
end

function module.cached_push_and_transform(constack, data, transfromation_picker)
    local top_layer = constack_last_layer[constack]
    local new_top = get_constack_layer_cache(data):get(top_layer, transfromation_picker)
    constack_last_layer[constack] = new_top
    constack_from_layer[new_top] = constack
    return top_layer, new_top
end

local table_cache_nil_placeholder = {}
module.table_cache_nil_placeholder = table_cache_nil_placeholder
local data_constack_table_cache = setmetatable({}, {__mode = "k"})
local function get_constack_table_cache(data)
    local data_key = data == nil and constack_value_tomb or data
    local cache = data_constack_table_cache[data_key]
    if cache == nil then
        cache = pcache.new(
            function(base_layer, transfromation_picker)
                assert(picker.is_picker(transfromation_picker))
                local modification = picker.eval_exhaustively(transfromation_picker, base_layer, data)
                assert(type(modification) == "table")
                local ret = {}
                for k, v in pairs(modification) do
                    if picker.is_picker(v) then
                        v = picker.eval_exhaustively(v, base_layer, data)
                    end
                    ret[k] = v == nil and table_cache_nil_placeholder or v
                end
                return ret
            end)
        data_constack_table_cache[data_key] = cache
    end
    return cache
end
function module.clear_constack_table_cache(data)
    data_constack_table_cache[data == nil and constack_value_tomb or data] = nil
end
function module.cached_table(constack, data, picker)
    local top_layer = constack_last_layer[constack]
    return get_constack_table_cache(data):get(top_layer, picker)
end

function module.push(constack)
    local ret = constack_last_layer[constack]
    local new_layer = create_layer(ret)
    constack_last_layer[constack] = new_layer
    constack_from_layer[new_layer] = constack
    return ret, new_layer
end

function module.restore(constack, to_layer)
    assert(constack_from_layer[to_layer] == constack, "cannot pop to layer not belonging to the constack")
    constack_last_layer[constack] = to_layer
end

function module.pop(constack)
    local to_layer = constack_last_layer[constack_last_layer[constack]]
    assert(type(to_layer) == "table", "cannot pop the base from constack")
    module.restore(constack, to_layer)
end

return module

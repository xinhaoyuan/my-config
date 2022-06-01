local gcache = require("gears.cache")
local beautiful = require("beautiful")
local unpack = unpack or table.unpack

local module = {}

local is_cached_picker = setmetatable({}, {__mode = "k"})
local picker_cache
picker_cache = gcache.new(
    function (type_ctor, ...)
        assert(type(type_ctor) == "function",
               ".type must be a type constructor")
        local ret = type_ctor(...)
        assert(type(ret) == "table" or type(ret) == "function")
        is_cached_picker[ret] = true
        return ret
    end)

module._constructors = {}

function module.new(value)
    if is_cached_picker[value] then return value end
    assert(type(value) == "table")
    assert(#value > 0)
    if type(value[1]) == "string" then value[1] = module._constructors[value[1]] end
    if type(value[1]) == "table" then
        value[1].preprocessor(value)
        value[1] = value[1].constructor
    end
    return picker_cache:get(unpack(value))
end

local constructor_cache = gcache.new(
    function (name)
        return function (args)
            return module.new({name, unpack(args)})
        end
    end)
local function get_constructor(name)
    if module._constructors[name] then
        return constructor_cache:get(name)
    end
end

function module._constructors.just(value)
    return function (context)
        return value
    end
end

function module._constructors.beautiful(...)
    local components = {...}
    return function (context)
        local ret = ""
        for _, component in ipairs(components) do
            if type(component) == "function" then
                component = component(context)
            end

            if component == nil then
                return nil
            elseif type(component) == "string" then
                ret = ret..component
            else
                print("ERROR: ignoring unknown color component "
                      ..tostring(component))
            end
        end
        return beautiful[ret]
    end
end

function module._constructors.switch(key, true_value, false_value)
    return function (context)
        local value
        if context[key] then
            value = true_value
        else
            value = false_value
        end
        if value == nil or type(value) == "string" then
            return value
        elseif type(value) == "function" then
            return value(context)
        else
            assert(false, "ignoring unknown switch value "..tostring(value))
        end
    end
end

module._constructors.fallback = {
    preprocessor = function (value)
        for i = 2, #value do
            value[i] = module.new(value[i])
        end
    end,
    constructor = function (...)
        local options = {...}
        return function (context)
            for _, option in ipairs(options) do
                local ret = option(context)
                if ret ~= nil then return ret end
            end
        end
    end,
}

module.none = get_constructor("just"){nil}
module.focus_switcher = get_constructor("switch"){"focus", "focus", "normal"}

return setmetatable(
    module, {
        __call = function (_, ...) return module.new(...) end,
        __index = function (_, name) return get_constructor(name) end,
    })

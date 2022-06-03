local gcache = require("gears.cache")
local beautiful = require("beautiful")
local unpack = unpack or table.unpack

local module = {}

local is_cached_picker = setmetatable({}, {__mode = "k"})
local picker_cache
picker_cache = gcache.new(
    function (type_ctor, ...)
        assert(type(type_ctor) == "function", "not a type constructor")
        local ret = type_ctor(...)
        assert(type(ret) == "table" or type(ret) == "function",
               "constructed picker must be a (callable) table or a function")
        is_cached_picker[ret] = true
        return ret
    end)
local function is_picker(value)
    if value ~= nil and is_cached_picker[value] then
        return true
    else
        return false
    end
end
module.is_picker = is_picker

local function eval_exhaustively(maybe_picker, constack, resolver)
    while true do
        if not is_picker(maybe_picker) and resolver ~= nil then
            maybe_picker = resolver(maybe_picker)
        end
        if is_picker(maybe_picker) then
            maybe_picker = maybe_picker(constack)
        else
            break
        end
    end
    return maybe_picker
end
module.eval_exhaustively = eval_exhaustively

module.constructors = {}

local function make_picker(value)
    if is_picker(value) then return value end
    assert(type(value) == "table" and #value > 0, "must be a non-empty list")
    if type(value[1]) == "string" then value[1] = module.constructors[value[1]] end
    if type(value[1]) == "table" and value[1].constructor then
        value[1] = value[1].constructor
    end
    return picker_cache:get(unpack(value))
end

local constructor_cache = gcache.new(
    function (name)
        return function (args, ...)
            assert(type(args) == "table" and #{...} == 0,
                   "must use constructor{} style")
            if type(module.constructors[name]) == "table" and
                module.constructors[name].preprocessor ~= nil then
                args = module.constructors[name].preprocessor(args)
            end
            return make_picker({name, unpack(args)})
        end
    end)
local function get_constructor(name)
    if module.constructors[name] then
        return constructor_cache:get(name)
    end
end

function module.constructors.just(value)
    return function (constack)
        return value
    end
end

function module.constructors.beautiful(...)
    local components = {...}
    return function (constack)
        local ret = ""
        for _, component in ipairs(components) do
            component = eval_exhaustively(component, constack)
            if component == nil then
                return nil
            elseif type(component) == "string" then
                ret = ret..component
            else
                print("ERROR: ignoring unknown beautiful key component "
                      ..tostring(component))
            end
        end
        return beautiful[ret]
    end
end

function module.constructors.constack(...)
    local components = {...}
    return function (constack)
        local ret = ""
        for _, component in ipairs(components) do
            component = eval_exhaustively(component, constack)
            if component == nil then
                return nil
            elseif type(component) == "string" then
                ret = ret..component
            else
                print("ERROR: ignoring unknown constack key component "
                      ..tostring(component))
            end
        end
        return constack[ret]
    end
end

function module.constructors.branch(cond, true_branch, false_branch)
    return function (constack)
        local value = eval_exhaustively(
            cond, constack, function (value)
                if type(value) == "string" then
                    return constack[value]
                end
                return value
            end)
        return value and true_branch or false_branch
    end
end

module.constructors.switch = {
    preprocessor = function (value)
        local result = {}
        for i = 2, #value do
            if type(value[i]) == "table" then
                assert(#value[i] == 2, "must have 2-pair for a branch")
                result[#result + 1] = value[i][1]
                result[#result + 1] = value[i][2]
            else
                assert(i == #value, "single value must be the last value")
                result[#result + 1] = value[i]
                break
            end
        end
        return result
    end,
    constructor = function (...)
        local args = {...}
        return function (constack)
            for i = 1, #args, 2 do
                local cond = eval_exhaustively(
                    args[i], constack, function (value)
                        if type(value) == "string" then
                            return constack[value]
                        end
                        return value
                    end)
                if cond then
                    return args[i + 1]
                end
            end
            if #args % 2 == 1 then
                return args[#args]
            end
        end
    end,
}

module.constructors.fallback = {
    preprocessor = function (value)
        for i = 1, #value do
            assert(is_picker(value[i]), "fallback takes a list of pickers")
        end
        return value
    end,
    constructor = function (...)
        local options = {...}
        return function (constack)
            for _, option in ipairs(options) do
                local ret = eval_exhaustively(option, constack)
                if ret ~= nil then return ret end
            end
        end
    end,
}

function module.constructors.wrap(f)
    return function (constack)
        return f(constack)
    end
end

module.none = get_constructor("just"){nil}
module.focus_switcher = get_constructor("branch"){"focus", "focus", "normal"}

return setmetatable(
    module, {
        __call = function (_, ...) return make_picker(...) end,
        __index = function (_, name) return get_constructor(name) end,
    })

local pcache = require((...):match("(.-)[^%.]+$").."pcache")
local beautiful = require("beautiful")
local unpack = unpack or table.unpack

local module = {}

local picker_info = setmetatable({}, {__mode = "k"})
local picker_cache
picker_cache = pcache.new(
    function (type_ctor, ...)
        assert(type(type_ctor) == "function", "not a type constructor")
        local ret, info = type_ctor(...)
        assert(type(ret) == "table" or type(ret) == "function",
               "constructed picker must be a (callable) table or a function")
        picker_info[ret] = info or true
        return ret
    end)
local function is_picker(value)
    if value ~= nil and picker_info[value] then
        return true
    else
        return false
    end
end
module.is_picker = is_picker

local function eval_exhaustively(maybe_picker, constack, data, resolver)
    local before_cycle_tracking = module.debug_eval_loop_count_before_cycle_tracking
    local reached
    while true do
        if not is_picker(maybe_picker) and resolver ~= nil then
            maybe_picker = resolver(maybe_picker)
        end
        if is_picker(maybe_picker) then
            if before_cycle_tracking then
                if before_cycle_tracking <= 0 then
                    if reached == nil then reached = {} end
                    assert(not reached[maybe_picker], "cycle detected when evaluating picker")
                    reached[maybe_picker] = true
                else
                    before_cycle_tracking = before_cycle_tracking - 1
                end
            end
            maybe_picker = maybe_picker(constack, data)
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

local constructor_cache = pcache.new(
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

local function eval_string(components, constack, data)
    local ret = ""
    for _, component in ipairs(components) do
        component = eval_exhaustively(component, constack, data)
        if component == nil then
            return nil
        elseif type(component) == "string" then
            ret = ret..component
        else
            print("WARNING: ignoring unknown string component "
                  ..tostring(component))
        end
    end
    return ret
end

function module.constructors.concat(...)
    local components = {...}
    return function (constack, data)
        return eval_string(components, constack, data)
    end
end

function module.constructors.beautiful(...)
    local components = {...}
    return function (constack, data)
        local ret = eval_string(components, constack, data)
        return ret and beautiful[ret]
    end
end

function module.constructors.constack(...)
    local components = {...}
    return function (constack, data)
        local ret = eval_string(components, constack, data)
        return ret and constack[ret]
    end
end

function module.constructors.data(...)
    local components = {...}
    return function (constack, data)
        if #components == 0 then return data end
        if type(data) ~= "table" then return nil end
        local ret = eval_string(components, constack, data)
        return ret and data[ret]
    end
end

function module.constructors.branch(cond, true_branch, false_branch)
    return function (constack, data)
        local value = eval_exhaustively(
            cond, constack, data, function (value)
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
        for i = 1, #value do
            assert(type(value[i]) == "table" and #value[i] == 2,
                   "must have 2-pair for a branch")
                result[#result + 1] = value[i][1]
                result[#result + 1] = value[i][2]
        end
        if value.default ~= nil then
            result[#result + 1] = value.default
        end
        return result
    end,
    constructor = function (...)
        local args = {...}
        return function (constack, data)
            for i = 1, #args, 2 do
                local cond = eval_exhaustively(
                    args[i], constack, data, function (value)
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
        return function (constack, data)
            for _, option in ipairs(options) do
                local ret = eval_exhaustively(option, constack, data)
                if ret ~= nil then return ret end
            end
        end
    end,
}

module.constructors.list = {
    constructor = function (...)
        local args = {...}
        return function (constack, data)
            local ret = {}
            for i = 1, #args do
                ret[i] = eval_exhaustively(args[i], constack, data)
            end
            return ret
        end
    end,
}

module.constructors.table = {
    preprocessor = function (value)
        assert(type(value) == "table")
        local keys = {}
        for k, _ in pairs(value) do
            keys[#keys + 1] = k
        end
        table.sort(keys)
        local result = {}
        for i = 1, #keys do
            result[#result + 1] = keys[i]
            result[#result + 1] = value[keys[i]]
        end
        return result
    end,
    constructor = function (...)
        local args = {...}
        return function (constack, data)
            local ret = {}
            for i = 1, #args, 2 do
                local key = eval_exhaustively(args[i], constack, data)
                local value = eval_exhaustively(args[i + 1], constack, data)
                ret[key] = value
            end
            return ret
        end
    end,
}

function module.constructors.wrap(f, ...)
    local args = {...}
    return function (constack, data)
        local args_eval = {}
        for i, arg in ipairs(args) do
            args_eval[i] = eval_exhaustively(arg, constack, data)
        end
        return f(unpack(args_eval))
    end
end

function module.constructors.wrap_raw(f)
    return function (constack, data)
        return f(constack, data)
    end
end

module.none = get_constructor("just"){nil}
module.highlighted_switcher = get_constructor("branch"){"highlighted", "focus", "normal"}

return setmetatable(
    module, {
        __call = function (_, ...) return make_picker(...) end,
        __index = function (_, name) return get_constructor(name) end,
    })

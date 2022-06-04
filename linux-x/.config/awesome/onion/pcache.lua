-- A fork of gears.cache without eviction.

local select = select                                                                          
local setmetatable = setmetatable
local unpack = unpack or table.unpack

local module = {}

function module:get(...)
    local result = self._cache
    for i = 1, select("#", ...) do
        local arg = select(i, ...)
        local next = result[arg]
        if not next then
            next = {}                                                                          
            result[arg] = next
        end
        result = next                                                                          
    end
    local ret = result._entry
    if not ret then
        ret = { self._creation_cb(...) }
        result._entry = ret
    end
    return unpack(ret)
end

function module.new(creation_cb)
    return setmetatable(
        {
            _cache = {},
            _creation_cb = creation_cb
        }, {
            __index = module
        })
end

return setmetatable(module, { __call = function(_, ...) return module.new(...) end })

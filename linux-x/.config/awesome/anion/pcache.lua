-- A fork of gears.cache without eviction: p(ersistent)cache.

local select = select
local setmetatable = setmetatable
local unpack = unpack or table.unpack

local pcache = {}

function pcache:get(...)
    local result = self._cache
    for i = 1, select("#", ...) do
        local arg = select(i, ...)
        if arg == nil then
            print(debug.traceback())
        end
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

function pcache.new(creation_cb)
    return setmetatable(
        {
            _cache = {},
            _creation_cb = creation_cb
        }, {
            __index = pcache
        })
end

return setmetatable(pcache, { __call = function(_, ...) return pcache.new(...) end })

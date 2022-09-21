-- P(ersistent)cache

local select = select
local setmetatable = setmetatable
local unpack = unpack or table.unpack
local function pack(...) return {n = select("#", ...), ...} end

local pcache = {}
local entry_key = {}
function pcache:get(...)
    local result = self._cache
    for i = 1, select("#", ...) do
        local arg = select(i, ...)
        if arg == nil then
            print("ERROR:", "got nil as pcache argument",
                  debug.traceback())
        end
        local next = result[arg]
        if not next then
            next = setmetatable({}, {__mode = "k"})
            result[arg] = next
        end
        result = next
    end
    local ret = result[entry_key]
    if ret == nil then
        ret = pack(self._creation_cb(...))
        result[entry_key] = ret
    end
    return unpack(ret, ret.n)
end

function pcache:clear(...)
    local n = select("#", ...)
    if n == 0 then
        self._cache = setmetatable({}, {__mode = "k"})
        return
    end
    local result = self._cache
    for i = 1, n do
        local arg = select(i, ...)
        if arg == nil then
            print("ERROR:", "got nil as pcache argument",
                  debug.traceback())
        end
        local next = result[arg]
        if not next or i == n then
            next = setmetatable({}, {__mode = "k"})
            result[arg] = next
        end
        result = next
    end
end

function pcache.new(creation_cb)
    return setmetatable(
        {
            _cache = setmetatable({}, {__mode = "k"}),
            _creation_cb = creation_cb
        }, {
            __index = pcache
        })
end

return setmetatable(pcache, { __call = function(_, ...) return pcache.new(...) end })

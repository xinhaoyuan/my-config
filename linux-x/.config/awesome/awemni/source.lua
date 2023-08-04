-- A source is an expandable list of objects (widgets or not).

local gobject = require("gears.object")

local array_proxy = {}

function array_proxy:get_is_append_only()
    return self._private.is_append_only
end

function array_proxy:get_children()
    return self._private.children
end

function array_proxy:get_array()
    return self._private.array
end

function array_proxy:set_array(array)
    local state = self._private
    state.is_append_only = false
    state.array = array
    self:emit_signal("property::children")
end

function array_proxy:set_size(size)
    local state = self._private
    state.size = size
    self:emit_signal("property::children")
end

function array_proxy:append(element)
    local state = self._private
    state.is_append_only = true
    state.array[#state.array + 1] = element
    self:emit_signal("property::children")
end

function array_proxy.new(args)
    args = args or {}
    local ret = gobject{enable_properties = true}
    local state
    state = {
        array = args.array or {},
        is_append_only = false,
        children = setmetatable(
            {}, {
                __index = function (_self, key)
                    return state.array[key]
                end,
                __len = function (_self)
                    return state.size or #state.array
                end,
            })
    }
    ret._private = state
    for k, v in pairs(array_proxy) do
        ret[k] = v
    end
    return ret
end

setmetatable(array_proxy, {__call = function (_self, ...) return array_proxy.new(...) end})

local function array_length(a)
    local mt = getmetatable(a)
    if mt and mt.__len then return mt.__len(a) end
    return #a
end

local filterable = {}

function filterable:reset_filtered_results()
    local state = self._private
    state.filtered_indices = {}
    state.filtered_upstream_size = 0
end

function filterable:update_for_appended_upstream()
    local state = self._private
    local upstream_children = state.upstream and state.upstream.children
    if upstream_children == nil then return end
    local prev_filtered_size = #state.filtered_indices
    local prev_upstream_size = state.filtered_upstream_size
    state.filtered_upstream_size = array_length(upstream_children)
    for i = prev_upstream_size + 1, state.filtered_upstream_size do
        local succ = state.callbacks.filter(state.filter, upstream_children[i])
        if succ then
            table.insert(state.filtered_indices, {i, succ})
        end
    end
    if state.callbacks.reduce then
        state.callbacks.reduce(state.filtered_indices)
    end
    if prev_filtered_size ~= #state.filtered_indices then
        self:emit_signal("property::children")
    end
end

function filterable:get_is_append_only()
    local state = self._private
    if state.is_append_only ~= nil then return state.is_append_only end
    local upstream = self._private.upstream
    return upstream and upstream.is_append_only
end

function filterable:get_input()
    return self._private.input
end

function filterable:get_filter()
    return self._private.filter
end

function filterable:set_input(input)
    local state = self._private
    if state.input == input then return end
    state.input = input
    state.filter = state.callbacks.pre_filter and state.callbacks.pre_filter(state.input) or state.input
    if state.upstream then state.upstream.input = input end
    state.is_append_only = false
    self:reset_filtered_results()
    self:emit_signal("property::input")
    self:update_for_appended_upstream()
    if #state.filtered_indices == 0 then
        self:emit_signal("property::children")
    end
    state.is_append_only = nil
end

function filterable:get_callbacks()
    return self._private.callbacks
end

function filterable:set_callbacks(callbacks)
    local state = self._private
    if state.callbacks == callbacks then return end
    state.callbacks = callbacks
    state.filter = state.callbacks.pre_filter and state.callbacks.pre_filter(state.input) or state.input
    state.is_append_only = false
    self:reset_filtered_results()
    self:emit_signal("property::callbacks")
    self:update_for_appended_upstream()
    if #state.filtered_indices == 0 then
        self:emit_signal("property::children")
    end
    state.is_append_only = nil
end

function filterable:set_upstream(upstream)
    local state = self._private
    if state.upstream then
        state.upstream:disconnect_signal("property::children", state.upstream_children_signal_handler)
    end
    state.upstream = upstream
    if upstream then
        upstream:connect_signal("property::children", state.upstream_children_signal_handler)
    end
    state.is_append_only = false
    self:reset_filtered_results()
    self:emit_signal("property::upstream")
    self:update_for_appended_upstream()
    if #state.filtered_indices == 0 then
        self:emit_signal("property::children")
    end
    state.is_append_only = nil
end

function filterable:get_children()
    return self._private.children
end

function filterable.new(args)
    local ret = gobject{
        enable_properties = true
    }
    local state
    state = {
        filtered_indices = {},
        callbacks = args.callbacks,
        children = setmetatable(
            {}, {
                __index = function (_self, key)
                    local use_post_filter = false
                    if type(key) == "number" then
                        key = state.filtered_indices[key]
                        key = key and key[1]
                        use_post_filter = true
                    end
                    if key == nil then return nil end
                    local upstream = state.upstream
                    if upstream == nil then return nil end
                    return use_post_filter and
                        state.callbacks.post_filter and
                        state.callbacks.post_filter(state.upstream.children[key]) or
                        upstream.children[key]
                end,
                __len = function ()
                    return #state.filtered_indices
                end,
            }),
        upstream_children_signal_handler = function ()
            if state.callbacks.reduce == nil and state.upstream.is_append_only then
                ret:update_for_appended_upstream()
            else
                ret:reset_filtered_results()
                ret:update_for_appended_upstream()
                if #state.filtered_indices == 0 then
                    ret:emit_signal("property::children")
                end
            end
        end
    }
    ret._private = state
    for k, v in pairs(filterable) do
        ret[k] = v
    end
    ret.upstream = args.upstream

    return ret
end

setmetatable(filterable, {__call = function (_self, ...) return filterable.new(...) end})

local concat = {}

function concat:set_upstreams(upstreams)
    upstreams = upstreams or {}
    local state = self._private
    for _, s in ipairs(state.upstreams) do
        s:disconnect_signal("property::children", self.update_from_upstreams)
    end
    state.upstreams = upstreams
    for _, s in ipairs(state.upstreams) do
        s:connect_signal("property::children", self.update_from_upstreams)
    end
    self.update_from_upstreams()
end

function concat:get_children()
    return self._private.children
end

function concat:set_input(input)
    local state = self._private
    for _, s in ipairs(state.upstreams) do
        s.input = input
    end
end

function concat.new(args)
    local ret = gobject{enable_properties = true}
    local state
    state = {
        upstreams = {},
        children = setmetatable(
            {}, {
                __index = function (_self, key)
                    if type(key) ~= "number" then return nil end
                    for i, s in ipairs(state.upstream_size) do
                        if key <= s then
                            return state.upstreams[i].children[key]
                        end
                        key = key - s
                    end
                    return nil
                end,
                __len = function ()
                    return state.size
                end,
            })
    }
    ret._private = state
    function ret.update_from_upstreams()
        state.upstream_size = {}
        state.size = 0
        for i, s in ipairs(state.upstreams) do
            state.upstream_size[i] = array_length(s.children)
            state.size = state.size + state.upstream_size[i]
        end
        ret:emit_signal("property::children")
    end
    for k, v in pairs(concat) do
        ret[k] = v
    end
    ret.upstreams = args.upstreams
    return ret
end

setmetatable(concat, {__call = function (_self, ...) return concat.new(...) end})

return {
    array_proxy = array_proxy,
    filterable = filterable,
    concat = concat,
}

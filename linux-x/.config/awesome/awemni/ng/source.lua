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

function filterable:reset_filter()
    local state = self._private
    state.filtered_indices = {}
    state.post_filtered = {}
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
        if state.callbacks.filter(state.filter, upstream_children[i]) then
            table.insert(state.filtered_indices, i)
        end
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

function filterable:set_input(input)
    local state = self._private
    if state.input == input then return end
    state.input = input
    state.filter = state.callbacks.pre_filter and state.callbacks.pre_filter(state.input) or state.input
    if state.upstream then state.upstream.input = intput end
    state.is_append_only = false
    self:reset_filter()
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
    self:reset_filter()
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
    self:reset_filter()
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
                __index = function (self, key)
                    local index = state.filtered_indices[key]
                    if index == nil then return nil end
                    local upstream = state.upstream
                    if upstream == nil then return nil end
                    if state.post_filtered[index] == nil then
                        state.post_filtered[index] = state.callbacks.post_filter and
                            state.callbacks.post_filter(state.upstream.children[index]) or upstream.children[index]
                    end
                    return state.post_filtered[index]
                end,
                __len = function ()
                    return #state.filtered_indices
                end,
            }),
        upstream_children_signal_handler = function ()
            if state.upstream.is_append_only then
                ret:update_for_appended_upstream()
            else
                ret:reset_filter()
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

return {
    array_proxy = array_proxy,
    filterable = filterable,
}

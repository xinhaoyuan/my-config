-- Listing widgets from a source with focus and execution.
--
-- For widgets, `focused` property use be used to indicate whether it is the focus;
-- `execute()` method will be called on execution.
--
-- TODO: document the access with the source.

local wibox = require("wibox")
local base = wibox.widget.base
local gobject = require("gears.object")
local gtable = require("gears.table")
local gtimer = require("gears.timer")

local entry_container = {}

function entry_container:layout(_context, width, height)
    for _, v in ipairs{"child", "placeholder"} do
        local widget = self._private[v]
        if widget then
            return {base.place_widget_at(widget, 0, 0, width, height)}
        end
    end
    return {}
end

function entry_container:fit(context, width, height)
    for _, v in ipairs{"child", "placeholder"} do
        local widget = self._private[v]
        if widget then
            return base.fit_widget(self, context, widget, width, height)
        end
    end
    return 0, 0
end

function entry_container:get_placeholder()
    return self._private.placeholder
end

function entry_container:set_placeholder(placeholder)
    self._private.placeholder = placeholder
    if self._private.child == nil then
        self:emit_signal("widget::redraw_needed")
        self:emit_signal("widget::layout_changed")
    end
end

function entry_container:get_children()
    return {self._private.child}
end

function entry_container:set_children(children)
    self._private.child = children[1]
    self:emit_signal("widget::redraw_needed")
    self:emit_signal("widget::layout_changed")
end

function entry_container:get_child()
    return self._private.child
end

function entry_container:set_child(child)
    self._private.child = child
    self:emit_signal("widget::redraw_needed")
    self:emit_signal("widget::layout_changed")
end

function entry_container:new()
    local ret = base.make_widget(nil, nil, {enable_properties = true})
    gtable.crush(ret, self, true)
    return ret
end

setmetatable(entry_container, {__call = function(self, ...) return self:new(...) end})

local function array_length(a)
    local mt = getmetatable(a)
    if mt and mt.__len then return mt.__len(a) end
    return #a
end

local lister = {}

function lister:start()
    local state = self._private
    state.about_to_start = false
    if state.source == nil then return end
    state.source.input = state.input
end

function lister:get_input()
    return self._private.input
end

function lister:set_input(input)
    local state = self._private
    if state.input ~= input then
        state.input = input
        state.executing_index = nil
        state.scrlist.anchor_index = nil
        state.about_to_start = true
        state.start_timer:again()
        self.focus = nil
    end
end

function lister:get_source()
    return self._private.source
end

function lister:set_source(source)
    local state = self._private
    if state.start_timer.started then
        state.start_timer:stop()
    else
        state.start_timer.started = true
    end
    if state.source then
        state.source:disconnect_signal("property::children", state.source_children_signal_handler)
    end
    state.source = source
    if source then
        source:connect_signal("property::children", state.source_children_signal_handler)
    end
    state.executing_index = nil
    self:emit_signal("property::source")
    self.focus = nil
    self:start()
end

function lister:set_focus(index)
    local state = self._private
    if state.about_to_start then
        state.focused_index = index
        return
    end
    if index then
        if index < 1 then index = 1 end
        if state.source.sealed_count and index >= state.source.sealed_count then
            index = state.source.sealed_count - 1
        end
    end

    local old_container = state.focused_index and state.scrlist.children[state.focused_index]
    state.scrlist.view_index = index
    state.focused_index = state.scrlist.view_index
    local new_container = state.focused_index and state.scrlist.children[state.focused_index]
    if old_container and old_container.child then
        old_container.child.focused = nil
    end
    if new_container and new_container.child then
        new_container.child.focused = true
    end
end

function lister:get_focus()
    return self._private.focused_index
end

function lister:get_focused_widget()
    local state = self._private
    local container = not state.about_to_start and state.scrlist.children[state.focused_index]
    return container and container.child
end

function lister:execute()
    local state = self._private
    local container = not state.about_to_start and state.scrlist.children[state.focused_index or 1]
    if container and container.child and container.child.execute then
        container.child:execute()
    elseif state.about_to_start then
        state.executing_index = 1
    elseif state.focused_index and state.source then
        state.executing_index = state.focused_index
    end
end

function lister:new(args)
    assert(type(args) == "table")
    assert(args.scrlist)

    local ret = gobject{enable_properties = true}
    local state
    state = {
        scrlist = args.scrlist,
        placeholder_widget = args.placeholder_widget,
        input = nil,
        requested_index = 0,
        focused_index = nil,
        source_length = 0,
        start_timer = gtimer{
            timeout = 0.05,
            single_shot = true,
            callback = function ()
                ret:start()
            end,
        },
    }
    function state.source_children_signal_handler()
        ret:emit_signal("property::source")
    end
    ret._private = state

    local children = setmetatable(
        {_containers = {}, _prev_size = 0}, {
            __index = function (self, index)
                if type(index) ~= "number" then return state.source[index] end
                if index < 1 or index > state.source_length then
                    if self._containers[index] then
                        -- For properly reset children focus later.
                        self._containers[index].child = nil
                    end
                    return nil
                end
                if self._containers[index] == nil then
                    local container = wibox.widget{
                        placeholder = state.placeholder_widget,
                        widget = entry_container,
                    }
                    container.index = index
                    container:connect_signal(
                        "button::release", function (self, _x, _y, b)
                            if b == 1 then
                                if ret.focus == self.index and container.child and container.child.execute then
                                    self.child:execute()
                                else
                                    ret.focus = self.index
                                end
                            end
                        end)
                    self._containers[index] = container
                end
                local old_child = self._containers[index].child
                local new_child = state.source.children[index]
                if old_child ~= new_child then
                    self._containers[index].child = new_child
                end
                if not state.about_to_start then
                    new_child.focused = index == state.focused_index
                end
                return self._containers[index]
            end,
            __len = function (_self)
                return state.source_length
            end,
        })
    state.scrlist.children = children
    ret:connect_signal(
        "property::source", function ()
            state.source_length = state.source and array_length(state.source.children) or 0
            if children._prev_size > state.source_length then
                for i = state.source_length, children._prev_size + 1, -1 do
                    children._containers[i] = nil
                end
            end
            children._prev_size = state.source_length
            state.scrlist:emit_signal("property::children")
            if state.source then
                local new_children = state.source.children
                state.scrlist.extended_count = new_children.extended_count
                if ret.focus == nil and new_children.focus then
                    ret.focus = new_children.focus
                end
                if state.executing_index and new_children[state.executing_index] then
                    local ei = state.executing_index
                    state.executing_index = nil
                    if new_children[ei].execute then new_children[ei]:execute() end
                end
            end
        end)

    for k, v in pairs(self) do
        ret[k] = v
    end
    ret.source = args.source

    return ret
end

return setmetatable(lister, {__call = function (self, ...) return self:new(...) end})

-- Lazy widget listing and filtering.

local wibox = require("wibox")
local base = wibox.widget.base
local gobject = require("gears.object")
local gtable = require("gears.table")
local gtimer = require("gears.timer")

local entry_container = {}

function entry_container:layout(context, width, height)
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

function entry_container:draw(...)
    self:emit_signal("draw")
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

function entry_container.new()
    local ret = base.make_widget(nil, nil, {enable_properties = true})
    gtable.crush(ret, entry_container, true)
    return ret
end

setmetatable(entry_container, {__call = function(_, ...) return entry_container.new(...) end})

local lister = {}

function lister:start()
    local state = self._private
    state.scrlist:reset()
    if state.source == nil then return end
    state.source:reset(
        state.input, function (extended_count)
            state.scrlist.extended_count = extended_count
        end)
    state.sealed_index = nil
    for i = 1, state.fetch_size do
        state.requested_index = i
        state.source:fetch_async(i, self.handle_widget)
    end
end

function lister:update_extended_count(extended_count)
    local state = self._private
    if extended_count == nil then extended_count = state.extended_count end
    if extended_index and state.sealed_index and state.sealed_index <= extended_index then
        extended_index = state.sealed_index - 1
    end
    state.scrlist.extended_count = extended_count
end

function lister:get_input()
    return self._private.input
end

function lister:set_input(input)
    local state = self._private
    if state.input ~= input then
        state.input = input
        state.executing_index = nil
        self.focus = nil
        state.start_timer:again()
    end
end

function lister:get_source()
    return self._private.source
end

function lister:set_source(source)
    local state = self._private
    if state.start_timer.started then
        state.start_timer:stop()
    end
    if state.source and state.source ~= source then
        state.source:reset()
    end
    state.source = source
    state.executing_index = nil
    self.focus = nil
    self:start()
end

function lister:set_focus(index)
    local state = self._private
    if state.start_timer.started then return nil end
    if index then
        if index < 1 then index = 1 end
        if state.sealed_index and index >= state.sealed_index then
            index = state.sealed_index - 1
        end
    end
    local old_container = state.scrlist.children[state.focused_index]
    state.focused_index = index
    local new_container = state.scrlist.children[state.focused_index]
    if old_container == new_container then return end
    if old_container and old_container.child then
        old_container.child.focused = nil
    end
    if new_container and new_container.child then
        new_container.child.focused = true
    end
    return index
end

function lister:get_focus()
    return self._private.focused_index
end

function lister:get_focused_widget()
    local state = self._private
    local container = not state.start_timer.started and state.scrlist.children[state.focused_index]
    return container and container.child
end

function lister:execute(...)
    local state = self._private
    local container = not state.start_timer.started and state.scrlist.children[state.focused_index or 1]
    if container and container.child then
        container.child:execute()
    elseif state.start_timer.started then
        state.executing_index = 1
    elseif state.focused_index and state.source then
        state.executing_index = state.focused_index
        state.source:fetch_async(state.focused_index, self.handle_widget)
    end
end

function lister:new(args)
    assert(type(args) == "table")
    assert(args.scrlist)
    args.fetch_size = args.fetch_size or 10
    assert(type(args.fetch_size) == "number" and args.fetch_size >= 1)

    local ret = gobject{enable_properties = true}
    local state
    state = {
        scrlist = args.scrlist,
        fetch_size = args.fetch_size,
        input = nil,
        source = nil,
        requested_index = 0,
        sealed_index = nil,
        focused_index = nil,
        start_timer = gtimer{
            timeout = 0.05,
            single_shot = true,
            callback = function () if state.source then ret:start() end end, 
        },
    }
    ret._private = state

    function ret.handle_widget(index, widget)
        if state.sealed_index and state.sealed_index <= index then return end
        if widget == nil then
            state.sealed_index = index
            ret:update_extended_count()
            if state.focused_index and state.focused_index >= index then
                self.focus = index - 1
            end
            return
        end
        while #state.scrlist.children < index do
            local current_index = #state.scrlist.children + 1
            local container = wibox.widget{
                placeholder = state.placeholder_widget,
                widget = entry_container,
            }
            container.index = current_index
            container:connect_signal(
                "draw", ret.container_on_draw)
            container:connect_signal(
                "button::release", function (self, _x, _y, b)
                    if b == 1 then
                        if ret.focus == self.index and container.child then
                            self.child:execute()
                        else
                            ret.focus = self.index
                        end
                    end
                end)
            container:connect_signal(
                "mouse::enter", function (self, _x, _y)
                    ret.focus = self.index
                end)
            table.insert(state.scrlist.children, container)
        end
        state.scrlist.children[index].child = widget
        state.scrlist:emit_signal("property::children")
        if index == state.focused_index then
            widget.focused = true
        end
        if index == state.executing_index then
            return widget:execute()
        end
    end

    function ret.container_on_draw(container)
        for i = state.requested_index + 1, container.index + state.fetch_size do
            if state.sealed_index and i >= state.sealed_index then break end
            state.requested_index = i
            state.source:fetch_async(i, ret.handle_widget)
        end
    end

    for k, v in pairs(self) do
        ret[k] = v
    end

    return ret
end

return setmetatable(lister, {__call = function (self, args) return self:new(args) end})

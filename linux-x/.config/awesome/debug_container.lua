local wibox = require("wibox")
local base = require("wibox.widget.base")
local gtable = require("gears.table")

local debug_container = { mt = {} }

function debug_container:layout(context, width, height)
    if self._private.debug_name then
        print(self._private.debug_name .. ": draw " .. tostring(self._private.widget)
              .. " w=" .. tostring(width) .. ", h=" .. tostring(height))
    end

    if self._private.widget then
        return {base.place_widget_at(self._private.widget, 0, 0, width, height)}
    else
        return {}
    end
end

function debug_container:fit(context, width, height)
    local w = 0
    local h = 0

    if self._private.widget then
        w, h = base.fit_widget(self, context, self._private.widget, width, height)
    end

    if self._private.debug_name then
        print(self._private.debug_name .. ": fit " .. tostring(self._private.widget)
              .. " w=" .. tostring(width) .. ", h=" .. tostring(height)
              .. " => w=" .. tostring(w) .. ", h=" .. tostring(h))
    end

    return w, h
end

function debug_container:get_children()
    return {self._private.widget}
end

function debug_container:set_children(children)
    self._private.widget = children[1]
    self:emit_signal("widget::layout_changed")
end

function debug_container:set_debug_name(prefix)
    self._private.debug_name = prefix
end

function debug_container:new(widget, debug_name)
    local ret = base.make_widget(nil, nil, {enable_properties = true})
    gtable.crush(ret, self, true)

    ret._private.widget = widget
    ret._private.debug_name = debug_name
    return ret
end

function debug_container.mt:__call(...)
    return self:new(...)
end

function debug_container.with_name(name)
    return function (widget)
        return wibox.widget {
            widget,
            debug_name = name,
            widget = debug_container
        }
    end
end

setmetatable(debug_container, debug_container.mt)

return debug_container

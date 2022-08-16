-- Make sure the width and height are integers.
local base = require("wibox.widget.base")
local gtable = require("gears.table")

local regulator = {}

function regulator:layout(context, width, height)
    if self._private.child == nil then return {} end
    local w, h = base.fit_widget(self, context, self._private.child, width, height)
    return {base.place_widget_at(self._private.child, 0, 0, w, h)}
end

function regulator:fit(context, width, height)
    if self._private.child == nil then return 0, 0 end
    local w, h = base.fit_widget(self, context, self._private.child, width, height)
    return math.ceil(w), math.ceil(h)
end

function regulator:get_children()
    return {self._private.child}
end

function regulator:set_children(children)
    self._private.child = children[1]
    self:emit_signal("widget::layout_changed")
end

function regulator.new(children, draw_last)
   local ret = base.make_widget(nil, nil, {enable_properties = true})
   gtable.crush(ret, regulator, true)
   return ret
end

return setmetatable(regulator, {__call = function (_self, ...) return regulator.new(...) end})

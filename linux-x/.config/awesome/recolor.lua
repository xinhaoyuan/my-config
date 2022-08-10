-- Recolor child content using the current source.
local base = require("wibox.widget.base")
local gtable = require("gears.table")

local recolor = {}

function recolor:before_draw_children(context, cr, width, height)
    if self._private.strength <= 0 then return end
    cr:push_group()
end

function recolor:after_draw_children(context, cr, width, height)
    if self._private.strength <= 0 then return end
    local pattern = cr:pop_group()
    cr:restore()
    cr:save()
    cr:push_group()
    cr:save()
    cr:set_source(pattern)
    cr:paint()
    cr:restore()
    cr:set_operator("HSL_COLOR")
    cr:paint_with_alpha(self._private.strength)
    cr:pop_group_to_source()
    cr:mask(pattern)
    select(2, pattern:get_surface()):finish()
end

function recolor:layout(context, width, height)
    if self._private.child == nil then return {} end
    return {base.place_widget_at(self._private.child, 0, 0, width, height)}
end

function recolor:fit(context, width, height)
    if self._private.child == nil then return 0, 0 end
    return base.fit_widget(self, context, self._private.child, width, height)
end

function recolor:get_children()
    return {self._private.child}
end

function recolor:set_children(children)
    self._private.child = children[1]
    self:emit_signal("widget::layout_changed")
end

function recolor:get_strength(s)
    return self._private.strength
end

function recolor:set_strength(s)
    s = s or 1
    if self._private.strength ~= s then
        self._private.strength = s
        self:emit_signal("widget::redraw_needed")
    end
end

function recolor.new(children, draw_last)
   local ret = base.make_widget(nil, nil, {enable_properties = true})
   gtable.crush(ret, recolor, true)
   gtable.crush(ret._private, {
                    strength = 1
                })
   return ret
end

return setmetatable(recolor, {__call = function (_self, ...) return recolor.new(...) end})

local base = require("wibox.widget.base")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local lgi = require("lgi")
local beautiful = require("beautiful")
local dpi = require("beautiful").xresources.apply_dpi

local scroller = { mt = {} }

function scroller:layout(context, width, height)
    if not self._private.widget then
        return
    end

    self:emit_signal("widget::redraw_needed")

    local content_width = math.max(width - self._private.scrollbar_width, 0)
    local w, h = base.fit_widget(self, context, self._private.widget, content_width, math.huge)
    local offset = math.max(math.min(self._private.offset, h - height), 0)
    -- Send signal?
    self._private.offset = offset
    if self._private.gravity == "top" then
        return { base.place_widget_at(self._private.widget, 0, -offset, content_width, h) }
    else
        return { base.place_widget_at(self._private.widget, 0, height - h + offset, content_width, h) }
    end
end

function scroller:fit(context, width, height)
    if not self._private.widget then
        return 0, 0
    end

    local w, h = base.fit_widget(self, context, self._private.widget, width, height)
    return w, h
end

local inactive_color
do
    local _, r, g, b, _ = gcolor(beautiful.fg_normal):get_rgba()
    inactive_color = lgi.cairo.SolidPattern.create_rgba(r, g, b, 0.25)
end
function scroller:draw(context, cr, width, height)
    local w, h
    if not self._private.widget then
        w, h = 0, 0
    end

    local content_width = width - self._private.scrollbar_width
    w, h = base.fit_widget(self, context, self._private.widget, content_width, math.huge)

    if h <= height then
        cr:set_source(inactive_color)
        cr:rectangle(content_width, 0, self._private.scrollbar_width, height)
        cr:fill()
    else
        local offset = self._private.offset
        local y_start = (self._private.gravity == "top" and offset or h - height - offset) / h * height
        local y_end = y_start + height * height / h
        cr:set_source(gcolor(beautiful.fg_normal))
        cr:rectangle(content_width, y_start, self._private.scrollbar_width, y_end - y_start)
        cr:fill()
    end
end

scroller.set_widget = base.set_widget_common

function scroller:get_widget()
    return self._private.widget
end

function scroller:get_children()
    return {self._private.widget}
end

function scroller:set_children(children)
    self:set_widget(children[1])
end

function scroller:reset()
    self:set_widget(nil)
end

function scroller:set_offset(value)
    value = value or 0
    if self._private.offset ~= value then
        self._private.offset = value
        self:emit_signal("widget::layout_changed")
    end
end

function scroller:get_offset()
    return self._private.offset
end

function scroller:set_gravity(value)
    value = value or "top"
    if self._private.gravity ~= value then
        self._private.gravity = value
        self:emit_signal("widget::layout_changed")
    end
end

function scroller:set_reverse_scrolling(value)
    self._private.reverse_scrolling = value
end

local function new(widget)
    local ret = base.make_widget(nil, nil, {enable_properties = true})

    gtable.crush(ret, scroller, true)

    ret._private.offset = 0
    ret._private.gravity = "top"
    ret._private.scrollbar_width = dpi(4)
    ret._private.reverse_scrolling = false
    ret:set_widget(widget)

    ret:connect_signal(
        "button::press",
        function(w, _x, _y, button)
            if w._private.reverse_scrolling then
                if button == 4 then
                    w.offset = w.offset + dpi(50)
                elseif button == 5 then
                    w.offset = w.offset - dpi(50)
                end
            else
                if button == 4 then
                    w.offset = w.offset - dpi(50)
                elseif button == 5 then
                    w.offset = w.offset + dpi(50)
                end
            end
        end
    )

    return ret
end

function scroller.mt:__call(_, ...)
    return new(_, ...)
end

return setmetatable(scroller, scroller.mt)

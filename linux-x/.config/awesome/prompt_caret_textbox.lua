-- A textbox hack to replace the block/underline cursor in awful.prompt with the in-between caret.
local base = require("wibox.widget.base")
local textbox = require("wibox.widget.textbox")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local gstring = require("gears.string")
local beautiful = require("beautiful")
local oconstack = require("onion.constack")
local lgi = require("lgi")
local dpi = require("beautiful.xresources").apply_dpi

local pct = {}

function pct:draw(context, cr, width, height)
    local cursor_pos = self._pct.cursor_pos
    local cursor_width = self._pct.cursor_width
    local ncr = {}
    ncr.mt = {}
    ncr.mt.__index = function(_, key)
        if key == "show_layout" then
            ncr[key] = function (self, layout)
                cr:save()
                cr:push_group_with_content("COLOR_ALPHA")
                cr:show_layout(layout)
                if cursor_pos then
                    local sp, wp = layout:get_cursor_pos(cursor_pos)
                    cr:save()
                    local x = sp.x / lgi.Pango.SCALE
                    if x + cursor_width > width then x = width - cursor_width end
                    cr:rectangle(x, sp.y / lgi.Pango.SCALE, cursor_width, sp.height / lgi.Pango.SCALE)
                    cr:set_operator("XOR")
                    cr:fill()
                    cr:restore()
                end
                cr:pop_group_to_source()
                cr:paint()
                cr:restore()
            end
        else
            ncr[key] = function (self, ...)
                cr[key](cr, ...)
            end
        end

        return ncr[key]
    end
    setmetatable(ncr, ncr.mt)
    self._pct.orig_draw(self, context, ncr, width, height)
end

function pct:fit(...)
    local w, h = self._pct.orig_fit(self, ...)
    if w == 0 and self._pct.cursor_pos then
        local _, logical = self._private.layout:get_pixel_extents()
        return self._pct.cursor_width, logical.height
    end
    return w, h
end

function pct:set_markup(markup)
    local cursor_pos = nil
    if markup then
        local s, e = markup:find("<span.*/span>")
        local at_end = e == #markup
        if s then
            local inner = at_end and "" or markup:match(">(.*)<")
            local before = markup:sub(1, s - 1)
            cursor_pos = string.wlen(gstring.xml_unescape(before))
            markup = before..inner..markup:sub(e + 1, at_end and -1 or -2)
        end
    end
    if cursor_pos ~= self._pct.cursor_pos then
        self._pct.cursor_pos = cursor_pos
        self:emit_signal("widget::redraw_needed")
    end
    self._pct.orig_set_markup(self, markup)
end

function pct:set_cursor_width(cw)
    if self._pct.cursor_width ~= cw then
        self._pct.cursor_width = cw
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function pct.wrap_widget(widget)
    if widget._pct then return widget end
    widget._pct = {
        cursor_width = dpi(1),
        orig_fit = widget.fit,
        orig_draw = widget.draw,
        orig_set_markup = widget.set_markup,
    }
    gtable.crush(widget, pct, true)
    return widget
end

function pct.new(...)
    return pct.wrap_widget(textbox(...))
end

return setmetatable(pct, {__call = function (self, ...) return pct.new(...) end})

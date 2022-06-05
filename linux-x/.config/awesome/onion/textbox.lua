-- A fork of wibox.widget.textbox with text/markup picker.
local textbox = require("wibox.widget.textbox")
local gtable = require("gears.table")
local oconstack = require((...):match("(.-)[^%.]+$").."constack")
local opicker = require((...):match("(.-)[^%.]+$").."picker")
local setmetatable = setmetatable

local module = {}

function module:update_with_picker(constack)
    local es = rawget(self, "emit_signal")
    rawset(self, "emit_signal", function () end)
    if self._private.markup_picker ~= nil then
        textbox.set_markup(
            self, opicker.eval_exhaustively(
                self._private.markup_picker, constack, self))
    elseif self._private.text_picker ~= nil then
        textbox.set_text(
            self, opicker.eval_exhaustively(
                self._private.text_picker, constack, self))
    end
    rawset(self, "emit_signal", es)
end

function module:fit(context, width, height)
    local constack = oconstack.get(context)
    self:update_with_picker(constack)
    return textbox.fit(self, constack, width, height)
end

function module:draw(context, cr, width, height)
    local constack = oconstack.get(context)
    self:update_with_picker(constack)
    return textbox.draw(self, constack, cr, width, height)
end

function module:set_markup_picker(picker)
    assert(picker == nil or opicker.is_picker(picker))
    if self._private.markup_picker ~= picker then
        self._private.markup_picker = picker
        self._private.text_picker = nil
        self:emit_signal("widget::redraw_needed")
        self:emit_signal("widget::layout_changed")
    end
end

function module:set_text_picker(picker)
    assert(picker == nil or opicker.is_picker(picker))
    if self._private.text_picker ~= picker then
        self._private.text_picker = picker
        self._private.markup_picker = nil
        self:emit_signal("widget::redraw_needed")
        self:emit_signal("widget::layout_changed")
    end
end

function module:get_text()
    if self._private.text_picker or self._private.markup_picker then
        return nil
    else
        return textbox.get_text(self)
    end
end

function module:get_markup()
    if self._private.text_picker or self._private.markup_picker then
        return nil
    else
        return textbox.get_markup(self)
    end
end

function module:new(...)
    local ret = textbox(...)
    gtable.crush(ret, self, true)
    return ret
end

return setmetatable(module, {__call = function(self, ...) return self:new(...) end})

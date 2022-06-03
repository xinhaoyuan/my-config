local base = require("wibox.widget.base")
local bg = require("wibox.container.background")
local gmath = require("gears.math")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local oconstack = require((...):match("(.-)[^%.]+$").."constack")
local opicker = require((...):match("(.-)[^%.]+$").."picker")
local unpack = unpack or table.unpack

local module = {}

function module:process_constack_before_children(constack)
    if self._private.context_transformation ~= nil then
        self._private.saved_constack = oconstack.push(constack)
        self:_apply_context_transformation(self._private.context_transformation, constack)
    end
end

function module:process_constack_after_children(constack)
    if self._private.context_transformation ~= nil then
        oconstack.pop(constack, self._private.saved_constack)
        self._private.saved_constack = nil
    end
end

function module:process_colors_before_children(constack)
    if self._private.bg_picker ~= nil then
        self._private.saved_background = self._private.background
        local background = opicker.eval_exhaustively(self._private.bg_picker, constack)
        self._private.background = background and gcolor(background)
    end
    if self._private.fg_picker ~= nil then
        self._private.saved_foreground = self._private.foreground
        local foreground = opicker.eval_exhaustively(self._private.fg_picker, constack)
        self._private.foreground = foreground and gcolor(foreground)
    end
end

function module:process_colors_after_children(constack)
    if self._private.bg_picker ~= nil then
        self._private.background = self._private.saved_background
        self._private.saved_background = nil
    end
    if self._private.fg_picker ~= nil then
        self._private.foreground = self._private.saved_foreground
        self._private.saved_foreground = nil
    end
end

function module:_apply_context_transformation(f, constack)
    if type(f) == "function" then
        f(constack)
    elseif type(f) == "table" then
        local target = self._private.context_transformation_has_picker and {} or constack
        for k, v in pairs(f) do
            if opicker.is_picker(v) then
                target[k] = opicker.eval_exhaustively(v, constack)
            else
                target[k] = v
            end
        end
        if target ~= constack then
            for k, v in pairs(target) do
                constack[k] = v
            end
        end
    end
end

function module:before_draw_children(context, cr, width, height)
    local constack = oconstack.get(context)
    self:process_constack_before_children(constack)
    self:process_colors_before_children(constack)
    bg.before_draw_children(self, constack, cr, width, height)
end

function module:after_draw_children(context, cr, width, height)
    local constack = oconstack.get(context)
    bg.after_draw_children(self, constack, cr, width, height)
    self:process_colors_after_children(constack)
    self:process_constack_after_children(constack)
end

function module:set_bg_picker(bg_picker)
    assert(bg_picker == nil or opicker.is_picker(bg_picker))
    if self._private.bg_picker ~= bg_picker then
        self._private.bg_picker = bg_picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_fg_picker(fg_picker)
    assert(fg_picker == nil or opicker.is_picker(fg_picker))
    if self._private.fg_picker ~= fg_picker then
        self._private.fg_picker = fg_picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_context_transformation(context_transformation)
    if self._private.context_transformation ~= context_transformation then
        self._private.context_transformation = context_transformation
        self._private.context_transformation_has_picker = nil
        if type(context_transformation) == "table" then
            for k, v in pairs(context_transformation) do
                if opicker.is_picker(v) then
                    self._private.context_transformation_has_picker = true
                    break
                end
            end
        end
        self:emit_signal("widget::redraw_needed")
        -- self:emit_signal("widget::layout_changed")
    end
end

function module:new(...)
    local ret = bg(...)
    gtable.crush(ret, self, true)
    return ret
end

function module.inject_beautiful(b)
    beautiful = b
end

return setmetatable(module, {__call = function(self, ...) return self:new(...) end})

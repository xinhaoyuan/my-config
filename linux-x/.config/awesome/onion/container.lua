local base = require("wibox.widget.base")
local bg = require("wibox.container.background")
local gmath = require("gears.math")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local ocontextack = require((...):match("(.-)[^%.]+$").."contextack")
local unpack = unpack or table.unpack

local module = {}

function module:process_contextack_before_children(contextack)
    if self._private.context_transformation ~= nil then
        self._private.saved_contextack = ocontextack.push(contextack)
        self:_apply_context_transformation(self._private.context_transformation, contextack)
    end
end

function module:process_contextack_after_children(contextack)
    if self._private.context_transformation ~= nil then
        ocontextack.pop(contextack, self._private.saved_contextack)
        self._private.saved_contextack = nil
    end
end

function module:process_colors_before_children(contextack)
    if self._private.bg_picker ~= nil then
        self._private.saved_background = self._private.background
        local background = self._private.bg_picker(contextack)
        self._private.background = background and gcolor(background)
    end
    if self._private.fg_picker ~= nil then
        self._private.saved_foreground = self._private.foreground
        local foreground = self._private.fg_picker(contextack)
        self._private.foreground = foreground and gcolor(foreground)
    end
end

function module:process_colors_after_children(contextack)
    if self._private.bg_picker ~= nil then
        self._private.background = self._private.saved_background
        self._private.saved_background = nil
    end
    if self._private.fg_picker ~= nil then
        self._private.foreground = self._private.saved_foreground
        self._private.saved_foreground = nil
    end
end

function module:_apply_context_transformation(f, contextack)
    if type(f) == "function" then
        f(contextack, self)
    elseif type(f) == "table" then
        for k, v in pairs(f) do
            contextack[k] = v
        end
    end
end

function module:before_draw_children(context, cr, width, height)
    local contextack = ocontextack.get(context)
    self:process_contextack_before_children(contextack)
    self:process_colors_before_children(contextack)
    bg.before_draw_children(self, contextack, cr, width, height)
end

function module:after_draw_children(context, cr, width, height)
    local contextack = ocontextack.get(context)
    bg.after_draw_children(self, contextack, cr, width, height)
    self:process_colors_after_children(contextack)
    self:process_contextack_after_children(contextack)
end

function module:set_bg_picker(bg_picker)
    if self._private.bg_picker ~= bg_picker then
        self._private.bg_picker = bg_picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_fg_picker(fg_picker)
    if self._private.fg_picker ~= fg_picker then
        self._private.fg_picker = fg_picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_context_transformation(context_transformation)
    if self._private.context_transformation ~= context_transformation then
        self._private.context_transformation = context_transformation
        self:emit_signal("widget::redraw_needed")
        -- self:emit_signal("widget::layout_changed")
    end
end

function module:new(...)
    local ret = bg(...)
    gtable.crush(ret, self, true)
    return ret
end

return setmetatable(module, {__call = function(self, ...) return self:new(...) end})

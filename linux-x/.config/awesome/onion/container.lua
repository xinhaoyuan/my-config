local base = require("wibox.widget.base")
local bg = require("wibox.container.background")
local gmath = require("gears.math")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local oconstack = require((...):match("(.-)[^%.]+$").."constack")
local opicker = require((...):match("(.-)[^%.]+$").."picker")
local unpack = unpack or table.unpack

local module = {}

local directions = {
    top    = "margin_top_picker",
    bottom = "margin_bottom_picker",
    left   = "margin_left_picker",
    right  = "margin_right_picker",
}

function module:process_constack_before(constack)
    if self._private.context_transformation ~= nil then
        self._private.saved_constack = oconstack.cached_push_and_transform(
            constack, self._private.context_transformation)
    end
end

function module:process_constack_after(constack)
    if self._private.context_transformation ~= nil then
        oconstack.restore(constack, self._private.saved_constack)
        self._private.saved_constack = nil
    end
end

function module:process_colors_before(constack)
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

function module:process_colors_after(constack)
    if self._private.bg_picker ~= nil then
        self._private.background = self._private.saved_background
        self._private.saved_background = nil
    end
    if self._private.fg_picker ~= nil then
        self._private.foreground = self._private.saved_foreground
        self._private.saved_foreground = nil
    end
end

function module:layout(context, width, height)
    if self._private.widget then
        local constack = oconstack.get(context)
        self:process_constack_before(constack)
        local l = self._private[directions.left]
        if l then l = opicker.eval_exhaustively(l, constack) else l = 0 end
        local r = self._private[directions.right]
        if r then r = opicker.eval_exhaustively(r, constack) else r = 0 end
        local t = self._private[directions.top]
        if t then t = opicker.eval_exhaustively(t, constack) else t = 0 end
        local b = self._private[directions.bottom]
        if b then b = opicker.eval_exhaustively(b, constack) else b = 0 end
        self:process_constack_after(constack)

        local resulting_width = width - l - r
        local resulting_height = height - t - b

        return {base.place_widget_at(self._private.widget,
                                     l, t,
                                     math.max(0, resulting_width),
                                     math.max(0, resulting_height))}
    end
end

function module:fit(context, width, height)
    local constack = oconstack.get(context)
    self:process_constack_before(constack)
    local extra_w = 0
    if self._private[directions.left] then
        extra_w = extra_w + opicker.eval_exhaustively(self._private[directions.left], constack)
    end
    if self._private[directions.right] then
        extra_w = extra_w + opicker.eval_exhaustively(self._private[directions.right], constack)
    end
    local extra_h = 0
    if self._private[directions.top] then
        extra_h = extra_h + opicker.eval_exhaustively(self._private[directions.top], constack)
    end
    if self._private[directions.bottom] then
        extra_h = extra_h + opicker.eval_exhaustively(self._private[directions.bottom], constack)
    end
    local w, h = 0, 0
    if self._private.widget then
        w, h = base.fit_widget(self, oconstack.get_last_layer(constack), self._private.widget, width - extra_w, height - extra_h)
    end
    self:process_constack_after(constack)
    if self._private.draw_empty == false and (w == 0 or h == 0) then
        return 0, 0
    end
    return w + extra_w, h + extra_h
end

function module:before_draw_children(context, cr, width, height)
    local constack = oconstack.get(context)
    self:process_constack_before(constack)
    self:process_colors_before(constack)
    bg.before_draw_children(self, constack, cr, width, height)
end

function module:after_draw_children(context, cr, width, height)
    local constack = oconstack.get(context)
    bg.after_draw_children(self, constack, cr, width, height)
    self:process_colors_after(constack)
    self:process_constack_after(constack)
end

function module:set_bg_picker(picker)
    assert(picker == nil or opicker.is_picker(picker))
    if self._private.bg_picker ~= picker then
        self._private.bg_picker = picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_fg_picker(picker)
    assert(picker == nil or opicker.is_picker(picker))
    if self._private.fg_picker ~= picker then
        self._private.fg_picker = picker
        self:emit_signal("widget::redraw_needed")
    end
end

function module:set_context_transformation(transformation)
    assert(transformation == nil or
           opicker.is_picker(transformation) or
           type(transformation) == "table")
    if type(transformation) == "table" and not opicker.is_picker(transformation) then
        transformation = opicker.table(transformation)
    end
    if self._private.context_transformation ~= transformation then
        self._private.context_transformation = transformation
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

for dir, key in pairs(directions) do
    module["set_"..key] = function(self, picker)
        if self._private[key] ~= picker then
            self._private[key] = picker
            self:emit_signal("widget::layout_changed")
        end
    end
end
function module:set_margin_pickers(o)
    assert(type(o) == "table")
    for dir, key in pairs(directions) do
        self[key] = o[dir]
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

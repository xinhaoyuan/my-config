local gtable = require("gears.table")
local oconstack = require((...):match("(.-)[^%.]+$").."constack")
local opicker = require((...):match("(.-)[^%.]+$").."picker")
local setmetatable = setmetatable

local onionize = {}

local function apply_pickers(widget, pickers, constack, emit_signal_override)
    local es = rawget(widget, "emit_signal")
    rawset(widget, "emit_signal", emit_signal_override)
    for _i, kv in ipairs(pickers) do
        if not kv[3] then widget[kv[1]] = opicker.eval_exhaustively(kv[2], constack, widget) end
    end
    for k, v in pairs(pickers) do
        if type(k) == "string" then
            widget[k] = opicker.eval_exhaustively(v, constack, widget)
        end
    end
    for _i, kv in ipairs(pickers) do
        if kv[3] then widget[kv[1]] = opicker.eval_exhaustively(kv[2], constack, widget) end
    end
    rawset(widget, "emit_signal", es)
end

local function apply_layout_pickers(widget, constack)
    local pickers = widget._onionize.layout_pickers
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, constact, function (self, name, ...)
            if name == "widget::layout_changed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function apply_draw_pickers(widget, constack)
    local pickers = widget._onionize.draw_pickers
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, constact, function (self, name, ...)
            if name == "widget::layout_changed" then
                print("WARNING", "Sending "..name.." while applying draw pickers")
                return
            elseif name == "widget::redraw_needed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function layout_override(self, context, width, height)
    local constack = oconstack.get(context)
    apply_layout_pickers(self, constack)
    return self._onionize.orig_layout(self, constack, width, height)
end

local function fit_override(self, context, width, height)
    local constack = oconstack.get(context)
    apply_layout_pickers(self, constack)
    return self._onionize.orig_fit(self, constack, width, height)
end

local function draw_override(self, context, cr, width, height)
    local constack = oconstack.get(context)
    apply_draw_pickers(self, constack)
    return self._onionize.orig_draw(self, constack, cr, width, height)
end

function onionize:set_layout_pickers(pickers)
    self._onionize.layout_pickers = pickers
    self:emit_signal("widget::layout_changed")
end

function onionize:set_draw_pickers(pickers)
    self._onionize.draw_pickers = pickers
    self:emit_signal("widget::redraw_needed")
end

function onionize.convert(widget)
    if widget._onionize then return widget end
    widget._onionize = {
        orig_draw = widget.draw,
        orig_fit = widget.fit,
        orig_layout = widget.layout,
        layout_pickers = widget.layout_pickers,
        draw_pickers = widget.draw_pickers,
    }
    widget.layout_pickers = nil
    widget.draw_pickers = nil
    gtable.crush(widget, onionize, true)
    if widget.fit then
        widget.fit = fit_override
    elseif widget.layout then
        widget.layout = layout_override
    end
    if widget.draw then
        widget.draw = draw_override
    end
    return widget
end

return setmetatable(onionize, {__call = function(_self, ...) return onionize.convert(...) end})

local gtable = require("gears.table")
local oconstack = require((...):match("(.-)[^%.]+$").."constack")
local opicker = require((...):match("(.-)[^%.]+$").."picker")
local setmetatable = setmetatable

local onionize = {}

local nil_placeholder = {}
onionize.nil_placeholder = nil_placeholder
local function apply_pickers(widget, pickers, constack, data, emit_signal_override)
    local es = rawget(widget, "emit_signal")
    rawset(widget, "emit_signal", emit_signal_override)
    assert(opicker.is_picker(pickers))
    local attrs = oconstack.cached_table(constack, data, pickers)
    for _i, kv in ipairs(attrs) do
        if not kv[3] then
            local k = kv[1]
            local v = kv[2]
            if v == nil_placeholder then v = nil end
            widget[k] = v
        end
    end
    for k, v in pairs(attrs) do
        if type(k) == "string" then
            if v == nil_placeholder then v = nil end
            widget[k] = v
        end
    end
    for _i, kv in ipairs(attrs) do
        if kv[3] then
            local k = kv[1]
            local v = kv[2]
            if v == nil_placeholder then v = nil end
            widget[k] = v
        end
    end
    rawset(widget, "emit_signal", es)
end

local function apply_layout_pickers(widget, constack, data)
    local pickers = widget._onionize.layout_pickers
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, constack, data, function (self, name, ...)
            if name == "widget::layout_changed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function apply_draw_pickers(widget, constack, data)
    local pickers = widget._onionize.draw_pickers
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, constack, data, function (self, name, ...)
            if name == "widget::layout_changed" then
                print("WARNING", "Sending "..name.." while applying draw pickers")
                return
            elseif name == "widget::redraw_needed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function layout_override(widget, context, width, height)
    local constack = oconstack.get(context)
    apply_layout_pickers(widget, constack, widget)
    return widget._onionize.orig_layout(widget, constack, width, height)
end

local function fit_override(widget, context, width, height)
    local constack = oconstack.get(context)
    apply_layout_pickers(widget, constack, widget)
    return widget._onionize.orig_fit(widget, constack, width, height)
end

local function draw_override(widget, context, cr, width, height)
    local constack = oconstack.get(context)
    apply_draw_pickers(widget, constack, widget)
    return widget._onionize.orig_draw(widget, constack, cr, width, height)
end

function onionize:set_layout_pickers(pickers)
    assert(pickers == nil or
           opicker.is_picker(pickers) or
           type(pickers) == "table")
    if type(pickers) == "table" and not opicker.is_picker(pickers) then
        pickers = opicker.table(pickers)
    end
    if self._onionize.layout_pickers ~= pickers then
        self._onionize.layout_pickers = pickers
        self:emit_signal("widget::layout_changed")
    end
end

function onionize:set_draw_pickers(pickers)
    assert(pickers == nil or
           opicker.is_picker(pickers) or
           type(pickers) == "table")
    if type(pickers) == "table" and not opicker.is_picker(pickers) then
        pickers = opicker.table(pickers)
    end
    if self._onionize.draw_pickers ~= pickers then
        self._onionize.draw_pickers = pickers
        self:emit_signal("widget::redraw_needed")
    end
end

local wrapped_constructor = {}
function onionize.wrap_constructor(ctor)
    local wrapped = wrapped_constructor[ctor]
    if wrapped == nil then
        wrapped = function (...)
            local widget = ctor(...)
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
            widget:connect_signal(
                "onion::widget_changed", function (self)
                    oconstack.clear_constack_table_cache(self)
                end)
            return widget
        end
        wrapped_constructor[ctor] = wrapped
        wrapped_constructor[wrapped] = wrapped
    end
    return wrapped_constructor[ctor]
end

return setmetatable(onionize, {__call = function(_self, ...) return onionize.wrap_constructor(...) end})

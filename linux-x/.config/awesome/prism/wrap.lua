local gtable = require("gears.table")
local pcache = require((...):match("(.-)[^%.]+$").."pcache")
local picker = require((...):match("(.-)[^%.]+$").."picker")
local setmetatable = setmetatable

local nil_value_tomb = {}
local data_table_cache = setmetatable({}, {__mode = "k"})
local function get_table_cache(data)
    local data_key = data == nil and nil_value_tomb or data
    local cache = data_table_cache[data_key]
    if cache == nil then
        cache = pcache.new(
            function(context, table_picker)
                assert(picker.is_picker(table_picker))
                local ret = picker.eval_exhaustively(table_picker, context, data)
                assert(type(ret) == "table")
                return ret
            end)
        data_table_cache[data_key] = cache
    end
    return cache
end
local function clear_table_cache(data)
    data_table_cache[data == nil and nil_value_tomb or data] = nil
end
local function cached_table(context, data, picker)
    return get_table_cache(data):get(context, picker)
end

local wrap = {}

local function apply_pickers(widget, pickers, context, data, emit_signal_override)
    local es = rawget(widget, "emit_signal")
    rawset(widget, "emit_signal", emit_signal_override)
    assert(picker.is_picker(pickers))
    local attrs = cached_table(context, data, pickers)
    for _i, kv in ipairs(attrs) do
        if not kv[3] then
            local k = kv[1]
            local v = kv[2]
            widget[k] = v
        end
    end
    for k, v in pairs(attrs) do
        if type(k) == "string" then
            widget[k] = v
        end
    end
    for _i, kv in ipairs(attrs) do
        if kv[3] then
            local k = kv[1]
            local v = kv[2]
            widget[k] = v
        end
    end
    rawset(widget, "emit_signal", es)
end

local function apply_pickers_before_layout(widget, context, pickers)
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, context, widget, function (self, name, ...)
            if name == "widget::layout_changed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function apply_pickers_before_draw(widget, context, pickers)
    if pickers == nil then return end
    local es = rawget(widget, "emit_signal")
    apply_pickers(
        widget, pickers, context, widget, function (self, name, ...)
            if name == "widget::layout_changed" then
                print("WARNING", "Sending "..name.." while applying draw pickers")
                return
            elseif name == "widget::redraw_needed" then
                return
            end
            return es(self, name, ...)
        end)
end

local function fit_override(widget, context, width, height)
    apply_pickers_before_layout(widget, context, widget._prism_wrap.layout_pickers)
    return widget._prism_wrap.orig_fit(widget, context, width, height)
end

local function layout_override(widget, context, width, height)
    apply_pickers_before_layout(widget, context, widget._prism_wrap.layout_pickers)
    apply_pickers_before_layout(widget, context, widget._prism_wrap.draw_pickers)
    if widget._prism_wrap.orig_layout then
        return widget._prism_wrap.orig_layout(widget, context, width, height)
    end
end

local function draw_override(widget, context, cr, width, height)
    apply_pickers_before_draw(widget, context, widget._prism_wrap.draw_pickers)
    if widget._prism_wrap.orig_draw then
        return widget._prism_wrap.orig_draw(widget, context, cr, width, height)
    end
end

function wrap:set_layout_pickers(pickers)
    assert(pickers == nil or
           picker.is_picker(pickers) or
           type(pickers) == "table")
    if type(pickers) == "table" and not picker.is_picker(pickers) then
        pickers = picker.table(pickers)
    end
    if self._prism_wrap.layout_pickers ~= pickers then
        self._prism_wrap.layout_pickers = pickers
        self:emit_signal("widget::layout_changed")
    end
end

function wrap:set_draw_pickers(pickers)
    assert(pickers == nil or
           picker.is_picker(pickers) or
           type(pickers) == "table")
    if type(pickers) == "table" and not picker.is_picker(pickers) then
        pickers = picker.table(pickers)
    end
    if self._prism_wrap.draw_pickers ~= pickers then
        self._prism_wrap.draw_pickers = pickers
        self:emit_signal("widget::redraw_needed")
    end
end

local wrapped_constructor = {}
function wrap.wrap_constructor(ctor)
    local wrapped = wrapped_constructor[ctor]
    if wrapped == nil then
        wrapped = function (...)
            local widget = ctor(...)
            widget._prism_wrap = {
                orig_draw = widget.draw,
                orig_fit = widget.fit,
                orig_layout = widget.layout,
                layout_pickers = widget.layout_pickers,
                draw_pickers = widget.draw_pickers,
            }
            widget.layout_pickers = nil
            widget.draw_pickers = nil
            gtable.crush(widget, wrap, true)
            if widget.fit then
                widget.fit = fit_override
            end
            widget.layout = layout_override
            widget.draw = draw_override
            widget:connect_signal(
                "prism::widget_changed", function (self)
                    clear_table_cache(self)
                end)
            return widget
        end
        wrapped_constructor[ctor] = wrapped
        wrapped_constructor[wrapped] = wrapped
    end
    return wrapped_constructor[ctor]
end

return setmetatable(wrap, {__call = function(_self, ...) return wrap.wrap_constructor(...) end})

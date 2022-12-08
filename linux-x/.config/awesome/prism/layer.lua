local base = require("wibox.widget.base")
local gmath = require("gears.math")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local pcache = require((...):match("(.-)[^%.]+$").."pcache")
local picker = require((...):match("(.-)[^%.]+$").."picker")

local layer_tomb = setmetatable({}, {__mode = "k"})
local layer_prev = setmetatable({}, {__mode = "k"})
local layer_mt = {
    __index = function (self, key)
        if layer_tomb[self][key] then return nil end
        local value = layer_prev[self][key]
        if value == nil then
            layer_tomb[self][key] = true
        else
            rawset(self, key, value)
        end
        return value
    end,
    __newindex = function (self, key, value)
        rawset(self, key, value)
        if value == nil then
            layer_tomb[self][key] = true
        end
    end,
}
local function create_layer(base_layer)
    local new_layer = setmetatable({}, layer_mt)
    layer_tomb[new_layer] = setmetatable({}, {__mode = "k"})
    layer_prev[new_layer] = base_layer
    return new_layer
end

local nil_tomb = {}
local layer_cache
layer_cache = pcache.new(
    function (widget_or_picker, context)
        local transformation_picker
        local data
        if picker.is_picker(widget_or_picker) then
            transformation_picker = widget_or_picker
        else
            transformation_picker = widget_or_picker._private.context_transformation
            assert(picker.is_picker(transformation_picker))
            data = widget_or_picker
        end
        local transformation = picker.eval_exhaustively(transformation_picker, context, data)
        assert(type(transformation) == "table")
        local direct = transformation.__direct
        local new_layer = direct and create_layer(context) or {}
        for _i, kv in ipairs(transformation) do
            local k = kv[1]
            local v = kv[2]
            new_layer[k] = direct and v == nil and nil_tomb or v
        end
        for k, v in pairs(transformation) do
            if type(k) == "string" then
                if v == nil_tomb then
                    new_layer[k] = v
                else
                    new_layer[k] = v
                end
            end
        end
        if direct then return new_layer end
        new_layer.__direct = true
        return layer_cache:get(picker.table(new_layer), context)
    end)
awesome.connect_signal("theme_changed", function () layer_cache:clear() end)

local layer = {}

function layer:push_context(context)
    if self._private.context_transformation ~= nil then
        return layer_cache:get(self, context)
    end
    return context
end

function layer:layout(context, width, height)
    if self._private.widget then
        return {base.place_widget_at(
                    self._private.widget, 0, 0, width, height
                )}
    end
end

function layer:fit(context, width, height)
    local w, h = 0, 0
    if self._private.widget then
        w, h = base.fit_widget(self, self:push_context(context), self._private.widget, width, height)
    end
    return w, h
end

function layer:set_widget(widget)
    if self._private.widget ~= widget then
        self._private.widget = widget
        self:emit_signal("widget::layout_changed")
    end
end

function layer:get_widget()
    return self._private.widget
end

function layer:set_children(children)
    self:set_widget(children[1])
end

function layer:get_children()
    return {self._private.widget}
end

function layer:set_context_transformation(transformation)
    assert(transformation == nil or
           picker.is_picker(transformation) or
           type(transformation) == "table")
    if type(transformation) == "table" and not picker.is_picker(transformation) then
        transformation = picker.table(transformation)
    end
    if self._private.context_transformation ~= transformation then
        self._private.context_transformation = transformation
        self:emit_signal("prism::widget_changed")
    end
end

local function patch_hierarchy_update()
    local whierarchy = require("wibox.hierarchy")
    local found = false
    local index = 1
    while true do
        local name, value = debug.getupvalue(whierarchy.update, index)
        if name == nil then break end
        if name == "hierarchy_update" then
            found = true
            debug.setupvalue(
                whierarchy.update, index, function (self, context, widget, ...)
                    if widget.push_context then
                        context = widget:push_context(context)
                    end
                    return value(self, context, widget, ...)
                end)
            break
        end
        index = index + 1
    end
    if not found then
        print("ERROR:", "Cannot find hierarchy_update to patch.")
    end
end

local function patch_hierarchy_draw()
    local whierarchy = require("wibox.hierarchy")
    local orig_draw = whierarchy.draw
    if orig_draw == nil then
        print("ERROR:", "Cannot find hierarchy.draw to patch")
        return
    end
    whierarchy.draw = function (self, context, ...)
        if self._widget.push_context then
            context = self._widget:push_context(context)
        end
        return orig_draw(self, context, ...)
    end
end

local wibox_patched = false
local function patch_wibox_if_needed()
    if wibox_patched then return end
    wibox_patched = true
    patch_hierarchy_update()
    patch_hierarchy_draw()
end

patch_wibox_if_needed()

function layer.new()
    local ret = base.make_widget(nil, nil, {enable_properties = true})
    gtable.crush(ret, layer, true)
    ret:connect_signal(
        "prism::widget_changed", function (self)
            layer_cache:clear(self)
            self:emit_signal("widget::layout_changed")
            self:emit_signal("widget::redraw_needed")
        end)
    return ret
end

return setmetatable(layer, {__call = function(_self, ...) return layer.new(...) end})

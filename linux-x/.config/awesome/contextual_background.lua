-- An enhanced wibox.container.background that uses/transforms context to determine bg/fg

local base = require("wibox.widget.base")
local bg = require("wibox.container.background")
local gmath = require("gears.math")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local beautiful = require("beautiful")
local unpack = unpack or table.unpack

local mod = {mt = {}}

function mod:before_draw_children(context, cr, width, height)
    if self._private.ctf ~= nil then
        self._private.saved_context = gtable.clone(context, false)
        self:apply_ctf(self._private.ctf, context)
    end
    if self._private.bgf ~= nil then
        self._private.saved_background = self._private.background
        self._private.background = gcolor(self:apply_bgf(self._private.bgf, context))
    end
    if self._private.fgf ~= nil then
        self._private.saved_foreground = self._private.foreground
        self._private.foreground = gcolor(self:apply_fgf(self._private.fgf, context))
    end
    bg.before_draw_children(self, context, cr, width, height)
end

function mod:after_draw_children(context, cr, width, height)
    bg.after_draw_children(self, context, cr, width, height)
    if self._private.saved_context ~= nil then
        gtable.crush(context, self._private.saved_context)
        for k, v in pairs(context) do
            context[k] = self._private.saved_context[k]
        end
        self._private.saved_context = nil
    end
    if self._private.saved_background then
        self._private.background = self._private.saved_background
        self._private.saved_background = nil
    end
    if self._private.saved_foreground then
        self._private.foreground = self._private.saved_foreground
        self._private.saved_foreground = nil
    end
end

function mod:apply_bgf(f, context)
    if type(f) == "function" then
        return f(context, self)
    elseif type(f) == "table" then
        f = self.focusable_color(unpack(f))
        return f(context)
    end
end

function mod:apply_fgf(f, context)
    if type(f) == "function" then
        return f(context, self)
    elseif type(f) == "table" then
        f = self.focusable_color(unpack(f))
        return f(context)
    end
end

function mod:apply_ctf(f, context)
    if type(f) == "function" then
        f(context, self)
    elseif type(f) == "table" then
        for k, v in pairs(f) do
            context[k] = v
        end
    end
end

function mod:set_bg_function(bgf)
    if self._private.bgf ~= bgf then
        self._private.bgf = bgf
        self:emit_signal("widget::redraw_needed")
    end
end

function mod:set_fg_function(fgf)
    if self._private.fgf ~= fgf then
        self._private.fgf = fgf
        self:emit_signal("widget::redraw_needed")
    end
end

function mod:set_context_transform_function(ctf)
    if self._private.ctf ~= ctf then
        self._private.ctf = ctf
        self:emit_signal("widget::redraw_needed")
    end
end

function mod:new(...)
    local ret = bg(...)

    gtable.crush(ret, self, true)

    return ret
end

function mod.mt:__call(...)
    return self:new(...)
end

local focusable_color_func = {}
function mod.focusable_color(prefix, suffix)
    prefix = prefix or ""
    suffix = suffix or ""
    if focusable_color_func[prefix] == nil then
        focusable_color_func[prefix] = {}
    end
    if focusable_color_func[prefix][suffix] == nil then
        focusable_color_func[prefix][suffix] = function (c)
            if c.focus then
                return beautiful[prefix .. "focus" .. suffix]
            else
                return beautiful[prefix .. "normal" .. suffix]
            end
        end
    end
    return focusable_color_func[prefix][suffix]
end

return setmetatable(mod, mod.mt)

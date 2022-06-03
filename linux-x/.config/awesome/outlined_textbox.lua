-- A textbox Hack to draw a outline.
local base = require("wibox.widget.base")
local textbox = require("wibox.widget.textbox")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local beautiful = require("beautiful")
local oconstack = require("onion.constack")
local lgi = require("lgi")

local mod = {mt = {}}

function mod:draw(context, cr, width, height)
    local outline_size = self._private.outline_size or 0
    local outline_color
    if oconstack.get(context)["focus"] then
        outline_color = self._private.outline_color_focus and gcolor(self._private.outline_color_focus) or gcolor(beautiful.bg_focus)
    else
        outline_color = self._private.outline_color and gcolor(self._private.outline_color) or gcolor(beautiful.bg_normal)
    end
    if outline_size == 0 or outline_color == nil then
        return self:orig_draw(context, cr, width, height)
    end

    local ncr = {}
    ncr.mt = {}
    ncr.mt.__index = function(_, key)
        if key == "show_layout" then
            ncr[key] = function (self, layout)
                local p = cr:copy_path()
                cr:save()
                cr:layout_path(layout)
                cr:set_source(outline_color)
                cr:set_line_width(outline_size)
                cr:set_line_join("ROUND")
                cr:stroke()
                cr:restore()
                cr:new_path()
                cr:append_path(p)
                cr:show_layout(layout)
            end
        else
            ncr[key] = function (self, ...)
                cr[key](cr, ...)
            end
        end

        return ncr[key]
    end
    setmetatable(ncr, ncr.mt)
    self:orig_draw(context, ncr, width, height)
end

function mod:set_outline_color(outline_color)
    if outline_color then
        self._private.outline_color = outline_color
    else
        self._private.outline_color = nil
    end
    self:emit_signal("widget::redraw_needed")
end

function mod:set_outline_color_focus(outline_color_focus)
    if outline_color_focus then
        self._private.outline_color_focus = outline_color_focus
    else
        self._private.outline_color_focus = nil
    end
    self:emit_signal("widget::redraw_needed")
end

function mod:set_outline_size(outline_size)
    if outline_size then
        self._private.outline_size = outline_size
    else
        self._private.outline_size = nil
    end
    self:emit_signal("widget::redraw_needed")
end

function mod:new(...)
    local ret = textbox(...)

    ret.orig_draw = ret.draw
    gtable.crush(ret, self, true)

    return ret
end

function mod.mt:__call(...)
    return self:new(...)
end

return setmetatable(mod, mod.mt)

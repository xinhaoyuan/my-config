-- Fix a flaw in place
local base = require("wibox.widget.base")
local place = require("wibox.container.place")
local gtable = require("gears.table")

local mod = {mt = {}}

function mod:fit(context, width, height)
    if not self._private.widget then
        return (self._private.fill_horizontal and width or 0), (self._private.fill_vertical and height or 0)
    end

    local w, h = base.fit_widget(self, context, self._private.widget, width, height)
    if self._private.horizontal_fit == false then w = 0 end
    if self._private.vertical_fit == false then h = 0 end

    return (self._private.fill_horizontal and width or w), (self._private.fill_vertical and height or h)
end

function mod:set_vertical_fit(f)
    self._private.vertical_fit = f
end

function mod:set_horizontal_fit(f)
    self._private.horizontal_fit = f
end

function mod:new(...)
    local ret = place(...)

    gtable.crush(ret, self, true)

    return ret
end

function mod.mt:__call(...)
    return self:new(...)
end

return setmetatable(mod, mod.mt)

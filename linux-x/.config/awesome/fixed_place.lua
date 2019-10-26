-- Fix a flaw in place
local base = require("wibox.widget.base")
local place = require("wibox.container.place")
local gtable = require("gears.table")

local mod = {mt = {}}

function mod:fit(context, width, height)
    if not self._private.widget then
        return 0, 0
    end

    local w, h = base.fit_widget(self, context, self._private.widget, width, height)

    return (self._private.fill_horizontal and width or w), (self._private.fill_vertical and height or h)
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

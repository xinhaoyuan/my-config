-- Fix a flaw in margin which make child updates stop propagating.

local base = require("wibox.widget.base")
local margin = require("wibox.container.margin")
local gtable = require("gears.table")

local mod = {mt = {}}

function mod:layout(_, width, height)
    if self._private.widget then
        local x = self._private.left
        local y = self._private.top
        local w = self._private.right
        local h = self._private.bottom

        local resulting_width = width - x - w
        local resulting_height = height - y - h

        return { base.place_widget_at(self._private.widget, x, y, math.max(0, resulting_width), math.max(0, resulting_height)) }
    end
end

function mod:new(...)
    local ret = margin(...)

    gtable.crush(ret, self, true)

    return ret
end

function mod.mt:__call(...)
    return self:new(...)
end


function mod.apply(margin)
   marign.layout = mod.layout
   return margin
end

return setmetatable(mod, mod.mt)

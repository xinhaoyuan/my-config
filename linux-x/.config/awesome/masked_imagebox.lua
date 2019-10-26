-- A textbox Hack to draw a outline.
local base = require("wibox.widget.base")
local imagebox = require("wibox.widget.imagebox")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local beautiful = require("beautiful")
local lgi = require("lgi")

local mod = {mt = {}}

function mod:draw(context, cr, width, height)
    if width == 0 or height == 0 or not self._private.default then return end

    -- Set the clip
    if self._private.clip_shape then
        cr:clip(self._private.clip_shape(cr, width, height, unpack(self._private.clip_args)))
    end

    if not self._private.resize_forbidden then
        -- Let's scale the image so that it fits into (width, height)
        local w, h = self._private.default.width, self._private.default.height
        local aspect = math.min(width / w, height / h)
        cr:scale(aspect, aspect)
    end

    if self._private.image then
        cr:mask_surface(self._private.image, 0, 0)
    end
end

function mod:new(...)
    local ret = imagebox(...)

    ret.orig_draw = ret.draw
    gtable.crush(ret, self, true)

    return ret
end

function mod.convert(ib)
    ib.orig_draw = ib.draw
    gtable.crush(ib, mod, true)
    return ib
end

function mod.mt:__call(...)
    return self:new(...)
end

return setmetatable(mod, mod.mt)

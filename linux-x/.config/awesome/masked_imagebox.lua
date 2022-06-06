-- A textbox Hack to draw the alpha channel of the image with the foreground color.
local imagebox = require("wibox.widget.imagebox")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local lgi = require("lgi")
local cairo = lgi.cairo

local mod = {mt = {}}

local function dispose_pattern(pattern)
    local status, s = pattern:get_surface()
    if status == "SUCCESS" then
        s:finish()
    end
end

function mod:draw(context, cr, width, height)
    if width == 0 or height == 0 or not self._private.default then return end
    cr:save()
    cr:push_group_with_content(cairo.Content.ALPHA)
    self:orig_draw(context, cr, width, height)
    local mask = cr:pop_group()
    cr:restore()
    cr:mask(mask)
    cr:fill()
    dispose_pattern(mask)
end

function mod:new(...)
    local ret = imagebox(...)
    ret.orig_draw = ret.draw
    ret.draw = mod.draw
    return ret
end

function mod.convert(ib)
    if ib.draw ~= mod.draw then
        ib.orig_draw = ib.draw
        ib.draw = mod.draw
    end
    return ib
end

function mod.mt:__call(...)
    return self:new(...)
end

return setmetatable(mod, mod.mt)

-- A textbox Hack to draw the alpha channel of the image with the foreground color.
local imagebox = require("wibox.widget.imagebox")
local gtable = require("gears.table")
local gcolor = require("gears.color")
local lgi = require("lgi")
local cairo = lgi.cairo

local masked_imagebox = {}

local function dispose_pattern(pattern)
    local status, s = pattern:get_surface()
    if status == "SUCCESS" then
        s:finish()
    end
end

function masked_imagebox:draw(context, cr, width, height)
    if width == 0 or height == 0 or not self._private.default then return end
    cr:save()
    cr:push_group_with_content(cairo.Content.ALPHA)
    self._masked_imagebox.orig_draw(self, context, cr, width, height)
    local mask = cr:pop_group()
    cr:restore()
    cr:mask(mask)
    dispose_pattern(mask)
end

function masked_imagebox.new(...)
    return masked_imagebox.convert(imagebox(...))
end

function masked_imagebox.convert(widget)
    if widget._masked_imagebox then return widget end
    widget._masked_imagebox = {
        orig_draw = widget.draw
    }
    widget.draw = masked_imagebox.draw
    return widget
end

return setmetatable(masked_imagebox, {__call = function (_self, ...) return masked_imagebox.new(...) end})

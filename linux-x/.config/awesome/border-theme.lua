-- Draw functions for enhanced

local gcache = require("gears.cache")
local gcolor = require("gears.color")
local gmatrix = require("gears.matrix")
local beautiful = require("beautiful")
local cairo = require("lgi").cairo

local mod = {}

mod["top"] = 1
mod["bottom"] = 2
mod["left"] = 4
mod["right"] = 8

function mod.directions(args)
    local ret = 0
    local dset = {}
    for _, v in pairs(args) do
        dset[v] = mod[v]
    end
    for _, v in pairs(dset) do
        ret = ret + v
    end
    return ret
end

-- Default theme as a template

local default_theme = {}

function default_theme:before_draw(context, cr)
    if self.inited == nil then
        self.line_width = beautiful.border_width
        self.padding = beautiful.border_outer_space + beautiful.border_width / 2
        self.size = beautiful.border_outer_space + beautiful.border_width + beautiful.border_inner_space
        self.inited = true
    end
    cr:save()
    cr:set_source(gcolor(beautiful.border_space))
    cr:paint()
    cr:set_line_width(self.line_width)
    cr:set_source(gcolor(context.color))
end

function default_theme:top_left(context, cr)
    cr:move_to(self.padding, self.size)
    cr:line_to(self.padding, self.padding)
    cr:line_to(self.size, self.padding)
    cr:stroke()
end

function default_theme:top_right(context, cr)
    cr:move_to(self.size - self.padding, self.size)
    cr:line_to(self.size - self.padding, self.padding)
    cr:line_to(0, self.padding)
    cr:stroke()
end

function default_theme:bottom_left(context, cr)
    cr:move_to(self.padding, 0)
    cr:line_to(self.padding, self.size - self.padding)
    cr:line_to(self.size, self.size - self.padding)
    cr:stroke()
end

function default_theme:bottom_right(context, cr)
    cr:move_to(self.size - self.padding, 0)
    cr:line_to(self.size - self.padding, self.size - self.padding)
    cr:line_to(0, self.size - self.padding)
    cr:stroke()
end

function default_theme:top(context, cr, length)
    if length <= 0 then
        return
    end
    cr:move_to(0, self.padding)
    cr:line_to(length, self.padding)
    cr:stroke()
end

default_theme.bottom = default_theme.top

function default_theme:left(context, cr, length)
    if length <= 0 then
        return
    end
    cr:move_to(self.padding, 0)
    cr:line_to(self.padding, length)
    cr:stroke()
end

default_theme.right = default_theme.left

function default_theme:after_draw(context, cr)
    cr:restore()
end

mod.theme = default_theme

function mod.draw(context, cr, width, height, directions)
    local theme = mod.theme
    theme:before_draw(context, cr)
    if directions == 15 then
        -- full box
        theme:top_left(context, cr)
        cr:translate(theme.size, 0)
        theme:top(context, cr, width - theme.size * 2)
        cr:translate(width - theme.size * 2, 0)
        theme:top_right(context, cr)
        cr:translate(0, theme.size)
        theme:right(context, cr, height - theme.size * 2)
        cr:translate(theme.size - width, 0)
        theme:left(context, cr, height - theme.size * 2)
        cr:translate(0, height - theme.size * 2)
        theme:bottom_left(context, cr)
        cr:translate(theme.size, 0)
        theme:bottom(context, cr, width - theme.size * 2)
        cr:translate(width - theme.size * 2, 0)
        theme:bottom_right(context, cr)
    elseif directions == 14 then
        -- no top
        cr:translate(width - theme.size, 0)
        theme:right(context, cr, height - theme.size)
        cr:translate(theme.size - width, 0)
        theme:left(context, cr, height - theme.size)
        cr:translate(0, height - theme.size)
        theme:bottom_left(context, cr)
        cr:translate(theme.size, 0)
        theme:bottom(context, cr, width - theme.size * 2)
        cr:translate(width - theme.size * 2, 0)
        theme:bottom_right(context, cr)
    elseif directions == 13 then
        -- no bottom
        theme:top_left(context, cr)
        cr:translate(theme.size, 0)
        theme:top(context, cr, width - theme.size * 2)
        cr:translate(width - theme.size * 2, 0)
        theme:top_right(context, cr)
        cr:translate(0, theme.size)
        theme:right(context, cr, height - theme.size)
        cr:translate(theme.size - width, 0)
        theme:left(context, cr, height - theme.size)
    elseif directions == 11 then
        -- no left
        theme:top(context, cr, width - theme.size)
        cr:translate(width - theme.size, 0)
        theme:top_right(context, cr)
        cr:translate(0, theme.size)
        theme:right(context, cr, height - theme.size * 2)
        cr:translate(theme.size - width, height - theme.size * 2)
        theme:bottom(context, cr, width - theme.size)
        cr:translate(width - theme.size, 0)
        theme:bottom_right(context, cr)
    elseif directions == 7 then
        -- no right
        theme:top_left(context, cr)
        cr:translate(theme.size, 0)
        theme:top(context, cr, width - theme.size)
        cr:translate(-theme.size, theme.size)
        theme:left(context, cr, height - theme.size * 2)
        cr:translate(0, height - theme.size * 2)
        theme:bottom_left(context, cr)
        cr:translate(theme.size, 0)
        theme:bottom(context, cr, width - theme.size)
    elseif directions == 5 then
        -- top + left
        theme:top_left(context, cr)
        cr:translate(theme.size, 0)
        theme:top(context, cr, width - theme.size)
        cr:translate(-theme.size, theme.size)
        theme:left(context, cr, height - theme.size)
    elseif directions == 9 then
        -- top + right
        theme:top(context, cr, width - theme.size)
        cr:translate(width - theme.size, 0)
        theme:top_right(context, cr)
        cr:translate(0, theme.size)
        theme:right(context, cr, height - theme.size)
    elseif directions == 6 then
        -- bottom + left
        theme:left(context, cr, height - theme.size)
        cr:translate(0, height - theme.size)
        theme:bottom_left(context, cr)
        cr:translate(theme.size, 0)
        theme:bottom(context, cr, width - theme.size)
    elseif directions == 10 then
        -- bottom + right
        cr:translate(width - theme.size, 0)
        theme:right(context, cr, height - theme.size)
        cr:translate(0, height - theme.size)
        theme:bottom_right(context, cr)
        cr:translate(theme.size - width, 0)
        theme:bottom(context, cr, width - theme.size)
    elseif directions == 1 then
        theme:top(context, cr, width)
    elseif directions == 2 then
        theme:bottom(context, cr, width)
    elseif directions == 4 then
        theme:left(context, cr, height)
    elseif directions == 8 then
        theme:right(context, cr, height)
    end
    theme:after_draw(context, cr)
end

return mod

-- Draw functions for enhanced borders

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

function default_theme:init()
    if self.inited == nil then
        local inner_space = beautiful.border_inner_space or 0
        local outer_space = beautiful.border_outer_space or 0
        self.line_width = beautiful.border_width - inner_space - outer_space
        self.padding = outer_space + self.line_width / 2
        self.size = beautiful.border_width
        self.inited = true
    end
end

function default_theme:before_draw(context, cr)
    self:init()
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

function default_theme:bottom(context, cr, length)
    if length <= 0 then
        return
    end
    cr:move_to(0, self.size - self.padding)
    cr:line_to(length, self.size - self.padding)
    cr:stroke()
end

function default_theme:left(context, cr, length)
    if length <= 0 then
        return
    end
    cr:move_to(self.padding, 0)
    cr:line_to(self.padding, length)
    cr:stroke()
end


function default_theme:right(context, cr, length)
    if length <= 0 then
        return
    end
    cr:move_to(self.size - self.padding, 0)
    cr:line_to(self.size - self.padding, length)
    cr:stroke()
end

function default_theme:after_draw(context, cr)
    cr:restore()
end

mod.default_theme = default_theme

------

local rounded_theme = {}

function rounded_theme:init()
    if self.inited == nil then
        self.size = beautiful.border_width
        self.outer_space = beautiful.border_outer_space
        self.inner_space = beautiful.border_inner_space
        self.space_color = gcolor(beautiful.border_space)
        self.outer_light_color = "#aaaaaa"
        self.outer_dark_color = "#666666"
        self.outer_light_gcolor = gcolor(self.outer_light_color)
        self.outer_dark_gcolor = gcolor(self.outer_dark_color)
        self.outer_transient_gcolor = gcolor("linear:0,0:"..tostring(self.size)..","..tostring(self.size)..":0,"..self.outer_light_color..":1,"..self.outer_dark_color)
        self.inited = true
    end
end

function rounded_theme:before_draw(context, cr)
    self:init()
    cr:save()
end

function rounded_theme:after_draw(context, cr)
    cr:restore()
end

function rounded_theme:top_left(context, cr)
    cr:rectangle(0, 0, self.size, self.size)
    cr:set_source(self.outer_light_gcolor)
    cr:fill()

    cr:arc(self.size, self.size, (self.size - self.outer_space + self.inner_space) / 2, math.pi, math.pi * 1.5)
    cr:line_to(self.size, self.size)
    cr:close_path()
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:arc(self.size, self.size, self.size - self.outer_space, math.pi, math.pi * 1.5)
    cr:line_to(self.size, self.size - self.inner_space)
    cr:arc_negative(self.size, self.size, self.inner_space, math.pi * 1.5, math.pi)
    cr:close_path()
    cr:fill()
end

function rounded_theme:top_right(context, cr)
    cr:rectangle(0, 0, self.size, self.size)
    cr:set_source(self.outer_transient_gcolor)
    cr:fill()

    cr:arc(0, self.size, (self.size - self.outer_space + self.inner_space) / 2, math.pi * -0.5, 0)
    cr:line_to(0, self.size)
    cr:close_path()
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:arc(0, self.size, self.size - self.outer_space, math.pi * -0.5, 0)
    cr:line_to(0, self.size)
    cr:arc_negative(0, self.size, self.inner_space, 0, math.pi * -0.5)
    cr:close_path()
    cr:fill()
end

function rounded_theme:bottom_left(context, cr)
    cr:rectangle(0, 0, self.size, self.size)
    cr:set_source(self.outer_transient_gcolor)
    cr:fill()

    cr:arc(self.size, 0, (self.size - self.outer_space + self.inner_space) / 2, math.pi * 0.5, math.pi)
    cr:line_to(self.size, 0)
    cr:close_path()
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:arc(self.size, 0, self.size - self.outer_space, math.pi * 0.5, math.pi)
    cr:line_to(self.size, 0)
    cr:arc_negative(self.size, 0, self.inner_space, math.pi, math.pi * 0.5)
    cr:close_path()
    cr:fill()
end

function rounded_theme:bottom_right(context, cr)
    cr:rectangle(0, 0, self.size, self.size)
    cr:set_source(self.outer_dark_gcolor)
    cr:fill()

    cr:arc(0, 0, (self.size - self.outer_space + self.inner_space) / 2, 0, math.pi * 0.5)
    cr:line_to(0, 0)
    cr:close_path()
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:arc(0, 0, self.size - self.outer_space, 0, math.pi * 0.5)
    cr:line_to(0, 0)
    cr:arc_negative(0, 0, self.inner_space, math.pi * 0.5, 0)
    cr:close_path()
    cr:fill()
end

function rounded_theme:top(context, cr, length)
    if length <= 0 then
        return
    end

    cr:rectangle(0, 0, length, self.size)
    cr:set_source(self.outer_light_gcolor)
    cr:fill()
    cr:rectangle(0, (self.size - self.inner_space + self.outer_space) / 2, length, (self.size + self.inner_space - self.outer_space) / 2)
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:rectangle(0, self.outer_space, length, self.size - self.outer_space - self.inner_space)
    cr:fill()
end

function rounded_theme:bottom(context, cr, length)
    if length <= 0 then
        return
    end

    cr:rectangle(0, 0, length, self.size)
    cr:set_source(self.outer_dark_gcolor)
    cr:fill()
    cr:rectangle(0, 0, length, (self.size + self.inner_space - self.outer_space) / 2)
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:rectangle(0, self.inner_space, length, self.size - self.outer_space - self.inner_space)
    cr:fill()
end

function rounded_theme:left(context, cr, length)
    if length <= 0 then
        return
    end

    cr:rectangle(0, 0, self.size, length)
    cr:set_source(self.outer_light_gcolor)
    cr:fill()
    cr:rectangle((self.size - self.inner_space + self.outer_space) / 2, 0, (self.size + self.inner_space - self.outer_space) / 2, length)
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:rectangle(self.outer_space, 0, self.size - self.outer_space - self.inner_space, length)
    cr:fill()
end


function rounded_theme:right(context, cr, length)
    if length <= 0 then
        return
    end

    cr:rectangle(0, 0, self.size, length)
    cr:set_source(self.outer_dark_gcolor)
    cr:fill()
    cr:rectangle(0, 0, (self.size + self.inner_space - self.outer_space) / 2, length)
    cr:set_source(self.space_color)
    cr:fill()

    cr:set_source(gcolor(context.color))
    cr:rectangle(self.inner_space, 0, self.size - self.outer_space - self.inner_space, length)
    cr:fill()
end

mod.rounded_theme = rounded_theme

------

function mod:draw(context, cr, width, height, directions)
    local theme = context.theme or self.default_theme
    theme:before_draw(context, cr)
    if directions == 15 then
        -- full box
        cr:save(); theme:top_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:top(context, cr, width - theme.size * 2); cr:restore()
        cr:translate(width - theme.size * 2, 0)
        cr:save(); theme:top_right(context, cr); cr:restore()
        cr:translate(0, theme.size)
        cr:save(); theme:right(context, cr, height - theme.size * 2); cr:restore()
        cr:translate(theme.size - width, 0)
        cr:save(); theme:left(context, cr, height - theme.size * 2); cr:restore()
        cr:translate(0, height - theme.size * 2)
        cr:save(); theme:bottom_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:bottom(context, cr, width - theme.size * 2); cr:restore()
        cr:translate(width - theme.size * 2, 0)
        cr:save(); theme:bottom_right(context, cr); cr:restore()
    elseif directions == 14 then
        -- no top
        cr:translate(width - theme.size, 0)
        cr:save(); theme:right(context, cr, height - theme.size); cr:restore()
        cr:translate(theme.size - width, 0)
        cr:save(); theme:left(context, cr, height - theme.size); cr:restore()
        cr:translate(0, height - theme.size)
        cr:save(); theme:bottom_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:bottom(context, cr, width - theme.size * 2); cr:restore()
        cr:translate(width - theme.size * 2, 0)
        cr:save(); theme:bottom_right(context, cr); cr:restore()
    elseif directions == 13 then
        -- no bottom
        cr:save(); theme:top_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:top(context, cr, width - theme.size * 2); cr:restore()
        cr:translate(width - theme.size * 2, 0)
        cr:save(); theme:top_right(context, cr); cr:restore()
        cr:translate(0, theme.size)
        cr:save(); theme:right(context, cr, height - theme.size); cr:restore()
        cr:translate(theme.size - width, 0)
        cr:save(); theme:left(context, cr, height - theme.size); cr:restore()
    elseif directions == 11 then
        -- no left
        cr:save(); theme:top(context, cr, width - theme.size); cr:restore()
        cr:translate(width - theme.size, 0)
        cr:save(); theme:top_right(context, cr); cr:restore()
        cr:translate(0, theme.size)
        cr:save(); theme:right(context, cr, height - theme.size * 2); cr:restore()
        cr:translate(theme.size - width, height - theme.size * 2)
        cr:save(); theme:bottom(context, cr, width - theme.size); cr:restore()
        cr:translate(width - theme.size, 0)
        cr:save(); theme:bottom_right(context, cr); cr:restore()
    elseif directions == 7 then
        -- no right
        cr:save(); theme:top_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:top(context, cr, width - theme.size); cr:restore()
        cr:translate(-theme.size, theme.size)
        cr:save(); theme:left(context, cr, height - theme.size * 2); cr:restore()
        cr:translate(0, height - theme.size * 2)
        cr:save(); theme:bottom_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:bottom(context, cr, width - theme.size); cr:restore()
    elseif directions == 5 then
        -- top + left
        cr:save(); theme:top_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:top(context, cr, width - theme.size); cr:restore()
        cr:translate(-theme.size, theme.size)
        cr:save(); theme:left(context, cr, height - theme.size); cr:restore()
    elseif directions == 9 then
        -- top + right
        cr:save(); theme:top(context, cr, width - theme.size); cr:restore()
        cr:translate(width - theme.size, 0)
        cr:save(); theme:top_right(context, cr); cr:restore()
        cr:translate(0, theme.size)
        cr:save(); theme:right(context, cr, height - theme.size); cr:restore()
    elseif directions == 6 then
        -- bottom + left
        cr:save(); theme:left(context, cr, height - theme.size); cr:restore()
        cr:translate(0, height - theme.size)
        cr:save(); theme:bottom_left(context, cr); cr:restore()
        cr:translate(theme.size, 0)
        cr:save(); theme:bottom(context, cr, width - theme.size); cr:restore()
    elseif directions == 10 then
        -- bottom + right
        cr:translate(width - theme.size, 0)
        cr:save(); theme:right(context, cr, height - theme.size); cr:restore()
        cr:translate(0, height - theme.size)
        cr:save(); theme:bottom_right(context, cr); cr:restore()
        cr:translate(theme.size - width, 0)
        cr:save(); theme:bottom(context, cr, width - theme.size); cr:restore()
    elseif directions == 1 then
        cr:save(); theme:top(context, cr, width); cr:restore()
    elseif directions == 2 then
        cr:save(); theme:bottom(context, cr, width); cr:restore()
    elseif directions == 4 then
        cr:save(); theme:left(context, cr, height); cr:restore()
    elseif directions == 8 then
        cr:save(); theme:right(context, cr, height); cr:restore()
    end
    theme:after_draw(context, cr);
end

return mod

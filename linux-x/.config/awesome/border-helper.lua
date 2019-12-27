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

local rot90_cache = gcache.new(function (width, height)
        local ret = cairo.Matrix()
        ret:init(0, -1, 1, 0, 0, height)
        return ret
end)

local rot180_cache = gcache.new(function (width, height)
        local ret = cairo.Matrix()
        ret:init(-1, 0, 0, -1, width, height)
        return ret
end)

local rot270_cache = gcache.new(function (width, height)
        local ret = cairo.Matrix()
        ret:init(0, 1, -1, 0, width, 0)
        return ret
end)

local function full_box(cr, width, height, padding)
    cr:move_to(padding, padding)
    cr:line_to(width - padding, padding)
    cr:line_to(width - padding, height - padding)
    cr:line_to(padding, height - padding)
    cr:close_path()
end

local function topless(cr, width, height, padding)
    cr:move_to(padding, 0)
    cr:line_to(padding, height - padding)
    cr:line_to(width - padding, height - padding)
    cr:line_to(width - padding, 0)
end

local function top_left(cr, width, height, padding)
    cr:move_to(width, padding)
    cr:line_to(padding, padding)
    cr:line_to(padding, height)
end

local function top(cr, width, height, padding)
    cr:move_to(0, padding)
    cr:line_to(width, padding)
end

function mod.draw(cr, width, height, color, directions)
    cr:set_source(gcolor(beautiful.border_space))
    cr:paint()
    local padding = beautiful.border_outer_space + beautiful.border_width / 2
    cr:save()
    if directions == 15 then
        full_box(cr, width, height, padding)
    elseif directions == 14 then
        topless(cr, width, height, padding)
    elseif directions == 13 then
        -- everything but bottom
        cr:transform(rot180_cache:get(width, height))
        topless(cr, width, height, padding)
    elseif directions == 11 then
        -- everything but left
        cr:transform(rot90_cache:get(width, height))
        topless(cr, height, width, padding)
    elseif directions == 7 then
        -- everything but right
        cr:transform(rot270_cache:get(width, height))
        topless(cr, height, width, padding)
    elseif directions == 5 then
        top_left(cr, width, height, padding)
    elseif directions == 9 then
        -- top + right
        cr:transform(rot270_cache:get(width, height))
        top_left(cr, height, width, padding)
    elseif directions == 6 then
        -- bottom + left
        cr:transform(rot90_cache:get(width, height))
        top_left(cr, height, width, padding)
    elseif directions == 10 then
        -- bottom + right
        cr:transform(rot180_cache:get(width, height))
        top_left(cr, width, height, padding)
    elseif directions == 1 then
        top(cr, width, height, padding)
    elseif directions == 2 then
        -- bottom
        cr:transform(rot180_cache:get(width, height))
        top(cr, width, height, padding)
    elseif directions == 4 then
        -- left
        cr:transform(rot90_cache:get(width, height))
        top(cr, height, width, padding)
    elseif directions == 8 then
        -- right
        cr:transform(rot270_cache:get(width, height))
        top(cr, height, width, padding)
    end
    cr:restore()
    cr:set_source(gcolor(color))
    cr:set_line_width(beautiful.border_width)
    cr:stroke()
end

return mod

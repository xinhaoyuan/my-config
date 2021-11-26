local gcache = require("gears.cache")
local gcolor = require("gears.color")
local gmatrix = require("gears.matrix")
local beautiful = require("beautiful")
local lgi = require("lgi")
local gdebug = require("gears.debug")
local gcache = require("gears.cache")
local cairo = lgi.cairo
-- Safe load for optional Rsvg module
local Rsvg = nil
do
    local success, err = pcall(function() Rsvg = lgi.Rsvg end)
    if not success then
        gdebug.print_warning(debug.traceback("Could not load Rsvg: " .. tostring(err)))
    end
end


local module = {}

local direction_bits = {}
direction_bits["top"] = 1
direction_bits["bottom"] = 2
direction_bits["left"] = 4
direction_bits["right"] = 8

function module.directions(args)
    local ret = 0
    for i = 1, #args do
        local b = direction_bits[args[i]]
        if b then ret = ret | b end
    end
    return ret
end

local schema = {
    top_left_border = {"string", "function"},
    top_right_border = {"string", "function"},
    bottom_left_border = {"string", "function"},
    bottom_right_border = {"string", "function"},
    top_border = {"string", "function"},
    bottom_border = {"string", "function"},
    left_border = {"string", "function"},
    right_border = {"string", "function"},

    top_left_shape = {"string", "function"},
    top_right_shape = {"string", "function"},
    bottom_left_shape = {"string", "function"},
    bottom_right_shape = {"string", "function"},
    top_shape = {"string", "function"},
    bottom_shape = {"string", "function"},
    left_shape = {"string", "function"},
    right_shape = {"string", "function"},

    top_space = "number",
    bottom_space = "number",
    left_space = "number",
    right_space = "number",

    top_size = "number",
    bottom_size = "number",
    left_size = "number",
    right_size = "number",
}

function module.create(args)
    local ret = setmetatable({}, {__index = module})
    for k, t in pairs(schema) do
        assert(args[k] ~= nil, k.." must exist")
        if type(t) == "string" then
            assert(type(args[k]) == t, k.." must be of type "..t)
            ret[k] = args[k]
        elseif type(t) == "table" then
            local matched = false
            local type_names = ""
            for i = 1, #t do
                if type(args[k]) == t[i] then
                    matched = true
                    break
                end
                type_names = type_names..(i == 1 and "" or ",")..tostring(t[i])
            end
            assert(matched, k.." must be one of type {"..type_names.."}")
            ret[k] = args[k]
        elseif type(t) == "function" then
            ret[k] = t(args[k])
        else
            assert(false, "unknown type in schema: "..tostring(t))
        end
    end
    return ret
end

local svg_handle = {}
local svg_cache = gcache(
    function (svg_path, is_shape, width, height)
        if Rsvg == nil then return end
        local handle = svg_handle[svg_path]
        if handle == nil then
            local err
            handle, err = Rsvg.Handle.new_from_file(svg_path)
            if err then
                print("Error loading "..filename..": "..tostring(err))
                return
            end
        end

        local surf = cairo.ImageSurface(is_shape and cairo.Format.A8 or cairo.Format.ARGB32, width, height)
        local cr = cairo.Context(surf)

        if not is_shape then
            handle:set_stylesheet(":root { --color: red; fill: var(--color); }");
        end
        handle:render_document(cr, Rsvg.Rectangle{x = 0, y = 0, width = width, height = height})

        return surf
    end)

function module:draw_part(context, cr, part_name, is_shape, width, height)
    if width <= 0 or height <= 0 then return end
    local part = self[part_name..(is_shape and "_shape" or "_border")]
    if type(part) == "string" then
        local surf = svg_cache:get(part, is_shape, width, height)
        if surf == nil then return end
        cr:set_source_surface(surf)
        cr:set_operator("OVER")
        cr:rectangle(0, 0, width, height)
        cr:fill()
    else
        cr:save()
        part(self, context, cr, is_shape, width, height)
        cr:restore()
    end
end

function module:draw(context, cr, is_shape, width, height, directions)
    if width ~= math.floor(width) or height ~= math.floor(height) then
        print("WARNING: Got non-integer sizes", width, height)
    end
    local top, bottom, left, right
    if type(directions) == "table" then
        top = directions["top"] and (is_shape and self.top_size or self.top_space) or 0
        bottom = directions["bottom"] and (is_shape and self.bottom_size or self.bottom_space) or 0
        left = directions["left"] and (is_shape and self.left_size or self.left_space) or 0
        right = directions["right"] and (is_shape and self.right_size or self.right_space) or 0
    else
        top = directions & 1 == 0 and 0 or (is_shape and self.top_size or self.top_space)
        bottom = directions & 2 == 0 and 0 or (is_shape and self.bottom_size or self.bottom_space)
        left = directions & 4 == 0 and 0 or (is_shape and self.left_size or self.left_space)
        right = directions & 8 == 0 and 0 or (is_shape and self.right_size or self.right_space)
    end
    cr:save()
    if is_shape then
        cr:rectangle(left, top, width - left - right, height - top - bottom)
        cr:fill()
    end
    self:draw_part(context, cr, "top_left", is_shape, left, top)
    cr:translate(left, 0)
    self:draw_part(context, cr, "top", is_shape, width - left - right, top)
    cr:translate(width - left - right, 0)
    self:draw_part(context, cr, "top_right", is_shape, right, top)
    cr:translate(0, top)
    self:draw_part(context, cr, "right", is_shape, right, height - top - bottom)
    cr:translate(right - width, 0)
    self:draw_part(context, cr, "left", is_shape, left, height - top - bottom)
    cr:translate(0, height - top - bottom)
    self:draw_part(context, cr, "bottom_left", is_shape, left, bottom)
    cr:translate(left, 0)
    self:draw_part(context, cr, "bottom", is_shape, width - left - right, bottom)
    cr:translate(width - left - right, 0)
    self:draw_part(context, cr, "bottom_right", is_shape, right, bottom)
    cr:restore()
end

module.presets = {}

local function dispose_pattern(pattern)
    local status, s = pattern:get_surface()
    if status == "SUCCESS" then
        s:finish()
    end
end

function module.presets.soft_relief(args)
    local shade_light_r, shade_light_g, shade_light_b, shade_light_a =
        gcolor.parse_color(args.light_shade)
    local shade_high_light_r, shade_high_light_g, shade_high_light_b, shade_high_light_a =
        gcolor.parse_color(args.high_light_shade or args.light_shade)
    local shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a =
        gcolor.parse_color(args.dark_shade)
    local shade_high_dark_r, shade_high_dark_g, shade_high_dark_b, shade_high_dark_a =
        gcolor.parse_color(args.high_dark_shade or args.dark_shade)
    local border_width = args.border_width
    local padding_width = args.padding_width or 0

    return module.create{
        top_left_border = function (self, context, cr, _is_shape, width, _height)
            local color_pattern = cairo.Pattern.create_mesh()
            local ctrl_length = width * 0.2652
            local mid_length = width * 0.7071
            color_pattern:begin_patch()
            color_pattern:move_to(0, width)
            color_pattern:curve_to(0, width - ctrl_length,
                                   width - mid_length - ctrl_length, width - mid_length + ctrl_length,
                                   width - mid_length, width - mid_length)
            color_pattern:line_to(width, width)
            color_pattern:line_to(width, width)
            color_pattern:set_corner_color_rgba(0, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            color_pattern:set_corner_color_rgba(1, shade_high_light_r, shade_high_light_g, shade_high_light_b, shade_high_light_a)
            color_pattern:set_corner_color_rgba(2, shade_high_light_r, shade_high_light_g, shade_high_light_b, shade_high_light_a)
            color_pattern:set_corner_color_rgba(3, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            color_pattern:end_patch()
            color_pattern:begin_patch()
            color_pattern:move_to(width - mid_length, width - mid_length)
            color_pattern:curve_to(width - mid_length + ctrl_length, width - mid_length - ctrl_length,
                                   width - ctrl_length, 0,
                                   width, 0)
            color_pattern:line_to(width, width)
            color_pattern:line_to(width, width)
            color_pattern:set_corner_color_rgba(0, shade_high_light_r, shade_high_light_g, shade_high_light_b, shade_high_light_a)
            color_pattern:set_corner_color_rgba(1, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            color_pattern:set_corner_color_rgba(2, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            color_pattern:set_corner_color_rgba(3, shade_high_light_r, shade_high_light_g, shade_high_light_b, shade_high_light_a)
            color_pattern:end_patch()

            cr:push_group_with_content(cairo.Content.ALPHA)
            local pattern = cairo.Pattern.create_radial(width, width, 0,
                                                        width, width, width)
            pattern:add_color_stop_rgba(0.5, shade_light_r, shade_light_g, shade_light_b, 1)
            pattern:add_color_stop_rgba(1, shade_light_r, shade_light_g, shade_light_b, 0)
            cr:move_to(width, width)
            cr:arc(width, width, width, -math.pi, -math.pi / 2)
            cr:set_source(pattern)
            cr:fill()
            local mask = cr:pop_group()

            cr:set_source(color_pattern)
            cr:mask(mask)

            dispose_pattern(mask)

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:move_to(width, width)
            cr:arc(width, width, self.left_size + border_width + padding_width, -math.pi, -math.pi / 2)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:move_to(width, width)
                cr:arc(width, width, self.left_size + padding_width, -math.pi, -math.pi / 2)
                cr:fill()
            end
        end,
        top_border = function (self, context, cr, _is_shape, width, height)
            local pattern = cairo.Pattern.create_linear(0, height, 0, 0)
            pattern:add_color_stop_rgba(0.5, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            pattern:add_color_stop_rgba(1, shade_light_r, shade_light_g, shade_light_b, 0)
            cr:set_source(pattern)
            cr:rectangle(0, 0, width, height)
            cr:fill()

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:rectangle(0, height - self.left_size - border_width - padding_width, width, self.left_size + border_width + padding_width)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:rectangle(0, height - self.left_size - padding_width, width, self.left_size + padding_width)
                cr:fill()
            end
        end,
        top_right_border = function (self, context, cr, _is_shape, width, _height)
            local color_pattern = cairo.Pattern.create_mesh()
            -- local ctrl_length = width * 0.5523
            -- color_pattern:begin_patch()
            -- color_pattern:move_to(0, 0)
            -- color_pattern:curve_to(ctrl_length, 0, width, width  - ctrl_length, width, width)
            -- color_pattern:line_to(0, width)
            -- color_pattern:line_to(0, width)
            -- color_pattern:set_corner_color_rgb(0, shade_light_r, shade_light_g, shade_light_b)
            -- color_pattern:set_corner_color_rgb(1, shade_dark_r, shade_dark_g, shade_dark_b)
            -- color_pattern:set_corner_color_rgb(2, shade_dark_r, shade_dark_g, shade_dark_b)
            -- color_pattern:set_corner_color_rgb(3, shade_light_r, shade_light_g, shade_light_b)
            -- color_pattern:end_patch()
            local ctrl_length = width * 0.2652
            local mid_length = width * 0.7071
            color_pattern:begin_patch()
            color_pattern:move_to(0, 0)
            color_pattern:curve_to(ctrl_length, 0, mid_length - ctrl_length, width - mid_length - ctrl_length, mid_length, width - mid_length)
            color_pattern:line_to(0, width)
            color_pattern:line_to(0, width)
            color_pattern:set_corner_color_rgb(0, shade_light_r, shade_light_g, shade_light_b)
            color_pattern:set_corner_color_rgba(1, shade_light_r, shade_light_g, shade_light_b, 0)
            color_pattern:set_corner_color_rgba(2, shade_light_r, shade_light_g, shade_light_b, 0)
            color_pattern:set_corner_color_rgb(3, shade_light_r, shade_light_g, shade_light_b)
            color_pattern:end_patch()
            color_pattern:begin_patch()
            color_pattern:move_to(mid_length, width - mid_length)
            color_pattern:curve_to(mid_length + ctrl_length, width - mid_length + ctrl_length, width, width - ctrl_length, width, width)
            color_pattern:line_to(0, width)
            color_pattern:line_to(0, width)
            color_pattern:set_corner_color_rgba(0, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            color_pattern:set_corner_color_rgb(1, shade_dark_r, shade_dark_g, shade_dark_b)
            color_pattern:set_corner_color_rgb(2, shade_dark_r, shade_dark_g, shade_dark_b)
            color_pattern:set_corner_color_rgba(3, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            color_pattern:end_patch()

            cr:push_group_with_content(cairo.Content.ALPHA)
            local mask_pattern = cairo.Pattern.create_radial(0, width, 0,
                                                             0, width, width)
            mask_pattern:add_color_stop_rgba(0.5, 0, 0, 0, shade_light_a)
            mask_pattern:add_color_stop_rgba(1, 0, 0, 0, 0)
            cr:move_to(0, width)
            cr:arc(0, width, width, -math.pi / 2, -math.pi / 4)
            cr:set_source(mask_pattern)
            cr:fill()
            mask_pattern = cairo.Pattern.create_radial(0, width, 0,
                                                       0, width, width)
            mask_pattern:add_color_stop_rgba(0.5, 0, 0, 0, shade_dark_a)
            mask_pattern:add_color_stop_rgba(1, 0, 0, 0, 0)
            cr:move_to(0, width)
            cr:arc(0, width, width, -math.pi / 4, 0)
            cr:set_source(mask_pattern)
            cr:fill()
            local mask = cr:pop_group()

            cr:set_source(color_pattern)
            cr:mask(mask)

            dispose_pattern(mask)

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:move_to(0, width)
            cr:arc(0, width, self.left_size + border_width + padding_width, -math.pi / 2, 0)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:move_to(0, width)
                cr:arc(0, width, self.left_size + padding_width, -math.pi / 2, 0)
                cr:fill()
            end
        end,
        left_border = function (self, context, cr, _is_shape, width, height)
            local pattern = cairo.Pattern.create_linear(width, 0, 0, 0)
            pattern:add_color_stop_rgba(0.5, shade_light_r, shade_light_g, shade_light_b, shade_light_a)
            pattern:add_color_stop_rgba(1, shade_light_r, shade_light_g, shade_light_b, 0)
            cr:set_source(pattern)
            cr:rectangle(0, 0, width, height)
            cr:fill()

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:rectangle(width - self.left_size - border_width - padding_width, 0, self.left_size + border_width + padding_width, height)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:rectangle(width - self.left_size - padding_width, 0, self.left_size + padding_width, height)
                cr:fill()
            end
        end,
        right_border = function (self, context, cr, _is_shape, width, height)
            local pattern = cairo.Pattern.create_linear(0, 0, width, 0)
            pattern:add_color_stop_rgba(0.5, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            pattern:add_color_stop_rgba(1, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            cr:set_source(pattern)
            cr:rectangle(0, 0, width, height)
            cr:fill()

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:rectangle(0, 0, self.left_size + border_width + padding_width, height)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:rectangle(0, 0, self.left_size + padding_width, height)
                cr:fill()
            end
        end,
        bottom_left_border = function (self, context, cr, _is_shape, width, _height)
            local color_pattern = cairo.Pattern.create_mesh()
            -- local ctrl_length = width * 0.5523
            -- color_pattern:begin_patch()
            -- color_pattern:move_to(0, 0)
            -- color_pattern:curve_to(0, ctrl_length, width - ctrl_length, width, width, width)
            -- color_pattern:line_to(width, 0)
            -- color_pattern:line_to(width, 0)
            -- color_pattern:set_corner_color_rgb(0, shade_light_r, shade_light_g, shade_light_b)
            -- color_pattern:set_corner_color_rgb(1, shade_dark_r, shade_dark_g, shade_dark_b)
            -- color_pattern:set_corner_color_rgb(2, shade_dark_r, shade_dark_g, shade_dark_b)
            -- color_pattern:set_corner_color_rgb(3, shade_light_r, shade_light_g, shade_light_b)
            -- color_pattern:end_patch()

            local ctrl_length = width * 0.2652
            local mid_length = width * 0.7071
            color_pattern:begin_patch()
            color_pattern:move_to(0, 0)
            color_pattern:curve_to(0, ctrl_length, width - mid_length - ctrl_length, mid_length - ctrl_length, width - mid_length, mid_length)
            color_pattern:line_to(width, 0)
            color_pattern:line_to(width, 0)
            color_pattern:set_corner_color_rgb(0, shade_light_r, shade_light_g, shade_light_b)
            color_pattern:set_corner_color_rgba(1, shade_light_r, shade_light_g, shade_light_b, 0)
            color_pattern:set_corner_color_rgba(2, shade_light_r, shade_light_g, shade_light_b, 0)
            color_pattern:set_corner_color_rgb(3, shade_light_r, shade_light_g, shade_light_b)
            color_pattern:end_patch()
            color_pattern:begin_patch()
            color_pattern:move_to(width - mid_length, mid_length)
            color_pattern:curve_to(width - mid_length + ctrl_length, mid_length + ctrl_length, width - ctrl_length, width, width, width)
            color_pattern:line_to(width, 0)
            color_pattern:line_to(width, 0)
            color_pattern:set_corner_color_rgba(0, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            color_pattern:set_corner_color_rgb(1, shade_dark_r, shade_dark_g, shade_dark_b)
            color_pattern:set_corner_color_rgb(2, shade_dark_r, shade_dark_g, shade_dark_b)
            color_pattern:set_corner_color_rgba(3, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            color_pattern:end_patch()

            cr:push_group_with_content(cairo.Content.ALPHA)
            local mask_pattern = cairo.Pattern.create_radial(width, 0, 0,
                                                             width, 0, width)
            mask_pattern:add_color_stop_rgba(0.5, 0, 0, 0, shade_light_a)
            mask_pattern:add_color_stop_rgba(1, 0, 0, 0, 0)
            cr:move_to(width, 0)
            cr:arc(width, 0, width, 3 * math.pi / 4, math.pi)
            cr:set_source(mask_pattern)
            cr:fill()
            mask_pattern = cairo.Pattern.create_radial(width, 0, 0,
                                                       width, 0, width)
            mask_pattern:add_color_stop_rgba(0.5, 0, 0, 0, shade_dark_a)
            mask_pattern:add_color_stop_rgba(1, 0, 0, 0, 0)
            cr:move_to(width, 0)
            cr:arc(width, 0, width, math.pi / 2, 3 * math.pi / 4)
            cr:set_source(mask_pattern)
            cr:fill()
            local mask = cr:pop_group()

            cr:set_source(color_pattern)
            cr:mask(mask)

            dispose_pattern(mask)

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:move_to(width, 0)
            cr:arc(width, 0, self.left_size + border_width + padding_width, math.pi / 2, math.pi)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:move_to(width, 0)
                cr:arc(width, 0, self.left_size + padding_width, math.pi / 2, math.pi)
                cr:fill()
            end
        end,
        bottom_border = function (self, context, cr, _is_shape, width, height)
            local pattern = cairo.Pattern.create_linear(shade_dark_r, shade_dark_g, shade_dark_b, height)
            pattern:add_color_stop_rgba(0.5, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            pattern:add_color_stop_rgba(1, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            cr:set_source(pattern)
            cr:rectangle(0, 0, width, height)
            cr:fill()

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:rectangle(0, 0, width, self.left_size + border_width + padding_width)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:rectangle(0, 0, width, self.left_size + padding_width)
                cr:fill()
            end
        end,
        bottom_right_border = function (self, context, cr, _is_shape, width, _height)
            local color_pattern = cairo.Pattern.create_mesh()
            local ctrl_length = width * 0.2652
            local mid_length = width * 0.7071
            color_pattern:begin_patch()
            color_pattern:move_to(width, 0)
            color_pattern:curve_to(width, ctrl_length,
                                   mid_length + ctrl_length, mid_length - ctrl_length,
                                   mid_length, mid_length)
            color_pattern:line_to(0, 0)
            color_pattern:line_to(0, 0)
            color_pattern:set_corner_color_rgba(0, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            color_pattern:set_corner_color_rgba(1, shade_high_dark_r, shade_high_dark_g, shade_high_dark_b, shade_high_dark_a)
            color_pattern:set_corner_color_rgba(2, shade_high_dark_r, shade_high_dark_g, shade_high_dark_b, shade_high_dark_a)
            color_pattern:set_corner_color_rgba(3, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            color_pattern:end_patch()
            color_pattern:begin_patch()
            color_pattern:move_to(mid_length, mid_length)
            color_pattern:curve_to(mid_length - ctrl_length, mid_length + ctrl_length,
                                   ctrl_length, width,
                                   0, width)
            color_pattern:line_to(0, 0)
            color_pattern:line_to(0, 0)
            color_pattern:set_corner_color_rgba(0, shade_high_dark_r, shade_high_dark_g, shade_high_dark_b, shade_high_dark_a)
            color_pattern:set_corner_color_rgba(1, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            color_pattern:set_corner_color_rgba(2, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            color_pattern:set_corner_color_rgba(3, shade_high_dark_r, shade_high_dark_g, shade_high_dark_b, shade_high_dark_a)
            color_pattern:end_patch()

            cr:push_group_with_content(cairo.Content.ALPHA)
            local pattern = cairo.Pattern.create_radial(0, 0, 0,
                                                        0, 0, width)
            pattern:add_color_stop_rgba(0.5, shade_dark_r, shade_dark_g, shade_dark_b, 1)
            pattern:add_color_stop_rgba(1, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            cr:move_to(0, 0)
            cr:arc(0, 0, width, 0, math.pi / 2)
            cr:set_source(pattern)
            cr:fill()
            local mask = cr:pop_group()

            cr:set_source(color_pattern)
            cr:mask(mask)

            dispose_pattern(mask)

            -- local pattern = cairo.Pattern.create_radial(0, 0, 0,
            --                                             0, 0, width)
            -- pattern:add_color_stop_rgba(0.5, shade_dark_r, shade_dark_g, shade_dark_b, shade_dark_a)
            -- pattern:add_color_stop_rgba(1, shade_dark_r, shade_dark_g, shade_dark_b, 0)
            -- cr:move_to(0, 0)
            -- cr:arc(0, 0, width, 0, math.pi / 2)
            -- cr:set_source(pattern)
            -- cr:fill()

            cr:set_source(gcolor(context.focus and beautiful.border_focus or beautiful.border_normal))
            cr:move_to(0, 0)
            cr:arc(0, 0, self.left_size + border_width + padding_width, 0, math.pi / 2)
            cr:fill()

            if padding_width > 0 then
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:move_to(0, 0)
                cr:arc(0, 0, self.left_size + padding_width, 0, math.pi / 2)
                cr:fill()
            end
        end,
        top_left_shape = function (self, context, cr, _is_shape, width, _height)
            cr:set_source_rgb(0, 0, 0)
            cr:move_to(width, width)
            cr:arc(width, width, width, -math.pi, -math.pi / 2)
            cr:fill()
        end,
        top_shape = function (self, context, cr, _is_shape, width, height)
            cr:set_source_rgb(0, 0, 0)
            cr:rectangle(0, 0, width, height)
            cr:fill()
        end,
        top_right_shape = function (self, context, cr, _is_shape, width, _height)
            cr:set_source_rgb(0, 0, 0)
            cr:move_to(0, width)
            cr:arc(0, width, width, -math.pi / 2, 0)
            cr:fill()
        end,
        left_shape = function (self, context, cr, _is_shape, width, height)
            cr:set_source_rgb(0, 0, 0)
            cr:rectangle(0, 0, width, height)
            cr:fill()
        end,
        right_shape = function (self, context, cr, _is_shape, width, height)
            cr:set_source_rgb(0, 0, 0)
            cr:rectangle(0, 0, width, height)
            cr:fill()
        end,
        bottom_left_shape = function (self, context, cr, _is_shape, width, _height)
            cr:set_source_rgb(0, 0, 0)
            cr:move_to(width, 0)
            cr:arc(width, 0, width, math.pi / 2, math.pi)
            cr:fill()
        end,
        bottom_shape = function (self, context, cr, _is_shape, width, height)
            cr:set_source_rgb(0, 0, 0)
            cr:rectangle(0, 0, width, height)
            cr:fill()
        end,
        bottom_right_shape = function (self, context, cr, _is_shape, width, _height)
            cr:set_source_rgb(0, 0, 0)
            cr:move_to(0, 0)
            cr:arc(0, 0, width, 0, math.pi / 2)
            cr:fill()
        end,
        top_space = args.shade_width + args.radius,
        bottom_space = args.shade_width + args.radius,
        left_space = args.shade_width + args.radius,
        right_space = args.shade_width + args.radius,
        top_size = args.radius,
        bottom_size = args.radius,
        left_size = args.radius,
        right_size = args.radius,
    }
end

return module

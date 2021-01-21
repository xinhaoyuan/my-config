local capi = {
   awesome = awesome,
   client = client,
   mouse = mouse,
}

local shared = require((...):match("(.-)[^%.]+$") .. "shared")
shared.client = {}

local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi
local gtimer = require("gears.timer")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local gdebug = require("gears.debug")
local machi = require("layout-machi")
local border = require("border-theme")
local cbg = require("contextual_background")
local masked_imagebox = require("masked_imagebox")
local fallback = require("fallback")
local cairo = require("lgi").cairo
local table_join = awful.util.table.join
local delayed = gtimer.delayed_call
local cgroup = require("cgroup")
local function get_cgroup_client(anchor_client)
    local r = {}
    for c in awful.client.iterate(function (c)
            return c.cgroup == anchor_client.cgroup
    end) do
        r[#r + 1] = c
    end
    return r
end
local cgroup_switcher = require("yams").create {
    keys = { ["Alt_L"] = "release_to_exit", ["Alt_R"] = "release_to_exit", ["Escape"] = "switch" },
    opacity_other = 1,
    panel = false
}

function shared.client.titlebar_toggle(c)
   if not c.has_titlebar_enabled then
      shared.client.titlebar_enable(c)
   else
      shared.client.titlebar_disable(c)
   end
end

function shared.client.titlebar_enable(c)
   c.has_titlebar_enabled = true
   shared.client.titlebar_show(c)
end

function shared.client.titlebar_disable(c)
   c.has_titlebar_enabled = false
   shared.client.titlebar_hide(c)
end

function shared.client.titlebar_show(c)
    c.has_titlebar = true
end

function shared.client.titlebar_hide(c)
    c.has_titlebar = false
end

function shared.client.decoration_show(c)
    if not c.has_decoration then
        local geo
        if c.decoration_created then geo = c:geometry() end
        for _, d in ipairs({"top", "bottom", "left", "right"}) do
            awful.titlebar.show(c, d)
        end
        if geo then c:geometry(geo) end
        c.has_decoration = true
    end
end

function shared.client.decoration_hide(c)
    if c.has_decoration then
        local geo
        if c.decoration_created then geo = c:geometry() end
        for _, d in ipairs({"top", "bottom", "left", "right"}) do
            awful.titlebar.hide(c, d)
        end
        if geo then c:geometry(geo) end
        c.has_decoration = false
    end
end

local function group_client_filter(c)
    if not awful.client.focus.filter(c) then return false end
    if c.cgroup ~= nil and c.cgroup.current_client ~= c then return false end
    for _, t in ipairs(c:tags()) do
        if t.selected then
            return true
        end
    end
    return false
end

function shared.client.toggle_grouping(c)
    if c.cgroup == nil then
        local sel = nil
        for other in awful.client.iterate(group_client_filter, nil, c.screen) do
            if (sel == nil or sel.focus_timestamp < other.focus_timestamp) and other ~= c and (c.cgroup == nil or not c.cgroup.clients[other])then
                sel = other
            end
        end
        if sel then
            cgroup.pull(sel, c)
            c.cgroup:switch(c)
        end
    else
        c.cgroup:detach(c)
    end
end

function shared.client.enlarge(c)
   if c.fullscreen then
   elseif c.minimized then
      c.minimized = false
   elseif not c.maximized then
      c.maximized = true
   else
      c.fullscreen = true
   end
end

function shared.client.shrink(c)
   if c.fullscreen then
      c.fullscreen = false
   elseif c.maximized then
      c.maximized = false
   elseif not c.minimized then
      c.minimized = true
   end
end

function shared.client.start_switcher(c, quick_mode)
    c.maximized = false
    c.maximized_vertical = false
    c.maximized_horizontal = false
    c.fullscreen = false
    c.floating = false
    c:raise()
    if quick_mode then
        delayed(
            function ()
                machi.switcher.start(c, {["Super_L"] = true})
            end
        )
    else
        delayed(
            function ()
                machi.switcher.start(c)
            end
        )
    end
end

local client_keys = table_join(
    awful.key({ "Mod4" }, "Escape", function (c)
            shared.waffle.show_client_waffle(c, { anchor = "client" })
    end),
    awful.key({ "Mod4" }, ".", function (c)
            shared.client.start_switcher(c, false)
    end),

    awful.key({ "Mod4" }, "Tab", function (c)
            shared.client.start_switcher(c, true)
    end),

    awful.key({ "Mod4" }, "Prior", shared.client.enlarge),
    awful.key({ "Mod4" }, "Next", shared.client.shrink),
    awful.key({ "Mod4" }, "=", function (c) c.ontop = not c.ontop end),
    awful.key({ "Mod4" }, "-", function (c) c.sticky = not c.sticky end),

    awful.key({ "Mod4" }, "'", function (c)
            if c.titlebar_style == "mini" then
                c.titlebar_style = "full"
            elseif c.titlebar_style == "full" then
                c.titlebar_style = "full_bottom"
            elseif c.titlebar_style == "full_bottom" then
                c.titlebar_style = "mini_bottom"
            else
                c.titlebar_style = "mini"
            end
    end),

    awful.key({ "Mod4" }, "g", function (c)
            shared.client.toggle_grouping(c)
    end),

    awful.key({ "Mod1" }, "Escape", function (c)
            if c.cgroup then cgroup_switcher.start{clients = get_cgroup_client(c)} end
    end),

    awful.key({ "Mod4" }, "f", function (c)
            if c.fullscreen then
            else
                if c.floating then
                    c.maximized = false;
                    c.maximized_vertical = false;
                    c.maximized_horizontal = false;
                end
                awful.client.floating.toggle(c);
            end
    end),

    awful.key({ "Mod4" }, "c", function (c) c:kill() end)
)

local client_buttons = table_join(
   awful.button({ }, 1, function (c) capi.client.focus = c; c:raise() end),
   awful.button({ "Mod4" }, 1, function (c) awful.mouse.client.move(c) end),
   awful.button({ "Mod4" }, 3, function (c)
         local _, cc = awful.placement.closest_corner(capi.mouse, {parent = c})
         awful.mouse.client.resize(c, cc)
   end),
   awful.button({ "Mod4" }, 4, shared.client.enlarge),
   awful.button({ "Mod4" }, 5, shared.client.shrink)
)

-- gain focus and raise before moving

awful.mouse.resize.set_mode("live")

awful.mouse.resize.add_enter_callback(
   function (c)
      c:emit_signal("request::activate", "mouse.move", {raise=false})
      c:raise()
   end, 'mouse.move')

awful.mouse.resize.add_enter_callback(
   function (c)
      c:emit_signal("request::activate", "mouse.resize", {raise=false})
      c:raise()
   end, 'mouse.resize')

local opposite_dir = {
   ["left"] = "right",
   ["right"] = "left",
   ["top"] = "bottom",
   ["bottom"] = "top",
}

-- function create_titlebars(c)
--     -- buttons for the titlebar
--     local buttons = awful.util.table.join(
--         awful.button({ }, 1, function()
--                 c:emit_signal("request::activate", "titlebar", {raise = true})
--                 awful.mouse.client.move(c)
--         end),
--         awful.button({ }, 2, function()
--                 shared.client.titlebar_toggle(c)
--         end),
--         awful.button({ }, 3, function()
--                 c:emit_signal("request::activate", "titlebar", {raise = true})
--                 local _, cc = awful.placement.closest_corner(capi.mouse, {parent = c})
--                 awful.mouse.client.resize(c, cc)
--         end),
--         awful.button({ }, 4,
--             function ()
--                 if not c.maximized then
--                     shared.client.enlarge(c)
--                 end
--             end
--         ),
--         awful.button({ }, 5,
--             function ()
--                 shared.client.shrink(c)
--             end
--         )
--     )

--     local titlewidget = awful.titlebar.widget.titlewidget(c)
--     titlewidget:set_font(beautiful.font)
--     local titlebar_container = wibox.widget {
--         { -- Left
--             awful.titlebar.widget.iconwidget(c),
--             buttons = buttons,
--             layout  = wibox.layout.fixed.horizontal
--         },
--         { -- Space
--             titlewidget,
--             buttons = buttons,
--             left = dpi(4),
--             widget = wibox.container.margin,
--         },
--         { -- Right
--             awful.titlebar.widget.floatingbutton (c),
--             awful.titlebar.widget.maximizedbutton(c),
--             awful.titlebar.widget.stickybutton   (c),
--             awful.titlebar.widget.ontopbutton    (c),
--             awful.titlebar.widget.closebutton    (c),
--             layout = wibox.layout.fixed.horizontal()
--         },
--         layout = wibox.layout.align.horizontal,
--     }

--     if shared.var.titlebar_position == "left" or shared.var.titlebar_position == "right" then
--         titlebar_container = wibox.container.rotate(titlebar_container, "west")
--     end

--     titlebar_container = wibox.widget {
--         titlebar_container,
--         [opposite_dir[shared.var.titlebar_position]] = beautiful.border_width,
--         color = capi.client.focus == c and beautiful.border_focus or beautiful.border_normal,
--         widget = wibox.container.margin,
--     }
--     awful.titlebar(
--         c,
--         {
--             position = shared.var.titlebar_position,
--             size = beautiful.titlebar_size + beautiful.border_width,
--             font = beautiful.font,
--         }
--     ):setup({ titlebar_container, widget = wibox.container.margin })
--     c.titlebar_container = titlebar_container
-- end

local border_top = border.directions{ "top", "left", "right" }
local function draw_tb_border_bgimage_top(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    local indicator = beautiful.border_radius and beautiful.border_radius_cut and capi.client.focus == c
    if beautiful.border_radius then
        cr:set_source(gcolor(beautiful.border_space))
        if indicator then
            gshape.rectangle(cr, width, height)
        else
            beautiful.rect_with_corners(cr, width, height + beautiful.border_radius, true, true, false, false, beautiful.border_radius)
        end
        cr:fill()
        cr:set_operator('ATOP')
    end
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_top)
    if indicator then
        cr:set_source(gcolor(beautiful.fg_normal))
        local indent = (2 - math.sqrt(2)) * (beautiful.border_radius) - beautiful.border_outer_space

        cr:move_to(beautiful.border_outer_space, beautiful.border_outer_space)
        cr:line_to(beautiful.border_outer_space, indent)
        cr:line_to(indent, beautiful.border_outer_space)
        cr:fill()

        cr:move_to(width - indent, beautiful.border_outer_space)
        cr:line_to(width - beautiful.border_outer_space, beautiful.border_outer_space)
        cr:line_to(width - beautiful.border_outer_space, indent)
        cr:fill()
    end
end


local border_bottom = border.directions{ "bottom", "left", "right" }
local function draw_tb_border_bgimage_bottom(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    local indicator = beautiful.border_radius and beautiful.border_radius_cut and capi.client.focus == c
    if beautiful.border_radius then
        cr:set_source(gcolor(beautiful.border_space))
        if indicator then
            gshape.rectangle(cr, width, height)
        else
            cr:translate(0, -beautiful.border_radius)
            beautiful.rect_with_corners(cr, width, height + beautiful.border_radius, false, false, true, true, beautiful.border_radius)
            cr:translate(0, beautiful.border_radius)
        end
        cr:fill()
        cr:set_operator('ATOP')
    end
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_bottom)
    if indicator then
        cr:set_source(gcolor(beautiful.fg_normal))
        local indent = (2 - math.sqrt(2)) * (beautiful.border_radius) - beautiful.border_outer_space

        cr:move_to(width - indent, height - beautiful.border_outer_space)
        cr:line_to(width - beautiful.border_outer_space, height - beautiful.border_outer_space)
        cr:line_to(width - beautiful.border_outer_space, height - indent)
        cr:fill()

        cr:move_to(indent, height - beautiful.border_outer_space)
        cr:line_to(beautiful.border_outer_space, height - beautiful.border_outer_space)
        cr:line_to(beautiful.border_outer_space, height - indent)
        cr:fill()
    end
end

local border_left = border.directions{ "left" }
local function draw_tb_border_bgimage_left(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_left)
end

local border_right = border.directions{ "right" }
local function draw_tb_border_bgimage_right(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_right)
end

local function apply_container_shape(client, shape, mini_titlebar_shape)
  local geo = client:geometry()

  local img = cairo.ImageSurface(cairo.Format.A1, geo.width, geo.height)
  local cr = cairo.Context(img)

  cr:save()

  cr:set_operator(cairo.Operator.CLEAR)
  cr:set_source_rgba(0,0,0,1)
  cr:paint()

  cr:set_operator(cairo.Operator.SOURCE)
  cr:set_source_rgba(1,1,1,1)
  if shape then
      shape(cr, geo.width, geo.height)
  else
      cr:rectangle(0, 0, geo.width, geo.height)
  end
  cr:fill()

  cr:restore()

  if mini_titlebar_shape then
      cr:save()
      cr:set_operator(cairo.Operator.CLEAR)
      cr:set_source_rgba(0,0,0,1)
      mini_titlebar_shape(cr, geo.width, geo.height)
      cr:fill()
      cr:restore()
  end

  client.client_container_shape = img._native
  img:finish()
end

local function update_shape(c)
    local mini_titlebar_shape
    local padding = (c.has_border and beautiful.border_outer_space * 2 or 0)
    if not c.has_titlebar then
        mini_titlebar_shape = nil
    elseif c.titlebar_style == "mini" then
        mini_titlebar_shape = function (cr, width, height)
            cr:save()
            cr:translate(width - beautiful.mini_titlebar_width - padding, 0)
            beautiful.rect_with_corners(cr,
                                        beautiful.mini_titlebar_width + padding,
                                        beautiful.titlebar_size + padding,
                                        false, false, false, beautiful.border_radius,
                                        beautiful.border_radius)
            cr:restore()
        end
    elseif c.titlebar_style == "mini_bottom" then
        mini_titlebar_shape = function (cr, width, height)
            cr:save()
            cr:translate(width - beautiful.mini_titlebar_width - padding, height - beautiful.titlebar_size - padding)
            beautiful.rect_with_corners(cr,
                                        beautiful.mini_titlebar_width + padding,
                                        beautiful.titlebar_size + padding,
                                        beautiful.border_radius, false, false, false,
                                        beautiful.border_radius)
            cr:restore()
        end
    end
    apply_container_shape(
        c,
        beautiful.border_radius and c.has_border and function (cr, width, height)
            cr:save()
            cr:translate(beautiful.border_width, beautiful.border_width)
            beautiful.rect_with_corners(cr,
                                        width - beautiful.border_width * 2,
                                        height - beautiful.border_width * 2,
                                        true, true, true, true,
                                        beautiful.border_radius and beautiful.border_radius - beautiful.border_width)
            cr:restore()
        end,
        mini_titlebar_shape
    )
end

local function delayed_update_shape(c)
    if c.container_shape_update_scheduled == nil then
        c.container_shape_update_scheduled = true
        gtimer.delayed_call(function ()
                if not c.valid then return end
                update_shape(c)
                c.container_shape_update_scheduled = nil
        end)
    end
end

-- Note that the awesome native titlebars ared used for both borders and titlebar here.
function decorate(c)
    if not c.valid then return end
    local geo
    if c.decoration_created then geo = c:geometry() end

    local tw
    local to
    if beautiful.border_radius == nil then
        tw = beautiful.border_width
        to = 0
    else
        tw = beautiful.border_radius
        to = tw - beautiful.border_width
    end

    if c.has_titlebar and c.titlebar_style == "mini" then
        local tw_top = beautiful.titlebar_size + (c.has_border and beautiful.border_outer_space * 2 or 0)
        local to_top = tw_top - (c.has_border and beautiful.border_width or 0)
        awful.titlebar(c,
                       {
                           position = "top",
                           size = tw_top,
                           bg = "#00000000",
                           bgimage = c.has_border and draw_tb_border_bgimage_top,
                       }
        ) : setup(
            {
                {
                    {
                        {
                            {
                                {

                                    awful.widget.clienticon(c),
                                    {
                                        image = beautiful.client_default_icon,
                                        widget = masked_imagebox,
                                    },
                                    widget = fallback,
                                },
                                halign = "center",
                                widget = wibox.container.place
                            },
                            shape = function (cr, width, height)
                                if not c.valid then return end
                                beautiful.rect_with_corners(cr, width, height, false, beautiful.border_radius and c.has_border, false, beautiful.border_radius, beautiful.border_radius and beautiful.border_radius - beautiful.border_outer_space)
                            end,
                            bg_function = function (context, cr, width, height)
                                return context["client"] == capi.client.focus and beautiful.border_focus or beautiful.border_normal
                            end,
                            fg_function = function (context, cr, width, height)
                                return context["client"] == capi.client.focus and beautiful.fg_focus or beautiful.fg_normal
                            end,
                            widget = cbg
                        },
                        margins = c.has_border and beautiful.border_outer_space or 0,
                        widget = wibox.container.margin
                    },
                    forced_height = beautiful.titlebar_size + (c.has_border and beautiful.border_outer_space * 2 or 0),
                    forced_width = beautiful.mini_titlebar_width + (c.has_border and beautiful.border_outer_space * 2 or 0),
                    widget = wibox.container.constraint
                },
                halign = "right",
                widget = wibox.container.place
            }
                 )
        c:titlebar_top(tw_top, to_top)
    elseif c.has_titlebar and c.titlebar_style == "full" then
        local tw_top = beautiful.titlebar_size + (c.has_border and beautiful.border_outer_space or 0)
        local to_top = 0
        awful.titlebar(c,
                       {
                           position = "top",
                           size = tw_top,
                           bg = "#00000000",
                           bgimage = c.has_border and draw_tb_border_bgimage_top,
                       }
        ) : setup(
            {
                {
                    {
                        {

                            awful.widget.clienticon(c),
                            {
                                image = beautiful.client_default_icon,
                                widget = masked_imagebox,
                            },
                            widget = fallback,
                        },
                        halign = "center",
                        widget = wibox.container.place
                    },
                    shape = function (cr, width, height)
                        if not c.valid then return end
                        beautiful.rect_with_corners(cr, width, height, beautiful.border_radius and c.has_border, beautiful.border_radius and c.has_border, false, false, beautiful.border_radius and beautiful.border_radius - beautiful.border_outer_space)
                    end,
                    bg_function = function (context, cr, width, height)
                        return context["client"] == capi.client.focus and beautiful.border_focus or beautiful.border_normal
                    end,
                    fg_function = function (context, cr, width, height)
                        return context["client"] == capi.client.focus and beautiful.fg_focus or beautiful.fg_normal
                    end,
                    widget = cbg
                },
                margins = c.has_border and beautiful.border_outer_space or 0,
                widget = wibox.container.margin
            }
                 )
        c:titlebar_top(tw_top, to_top)
    elseif c.has_border then
        awful.titlebar(c,
                       {
                           position = "top",
                           size = tw,
                           bg = "#00000000",
                           bgimage = draw_tb_border_bgimage_top,
                       }
        ) : setup({ widget = wibox.container.background })
        c:titlebar_top(tw, to)
    else
        c:titlebar_top(0, 0)
    end

    if c.has_titlebar and c.titlebar_style == "mini_bottom" then
        local tw_bottom = beautiful.titlebar_size + (c.has_border and beautiful.border_outer_space * 2 or 0)
        local to_bottom = tw_bottom - (c.has_border and beautiful.border_width or 0)
        awful.titlebar(c,
                       {
                           position = "bottom",
                           size = tw_bottom,
                           bg = "#00000000",
                           bgimage = c.has_border and draw_tb_border_bgimage_bottom,
                       }
        ) : setup(
            {
                {
                    {
                        {
                            {
                                {

                                    awful.widget.clienticon(c),
                                    {
                                        image = beautiful.client_default_icon,
                                        widget = masked_imagebox,
                                    },
                                    widget = fallback,
                                },
                                halign = "center",
                                widget = wibox.container.place
                            },
                            shape = function (cr, width, height)
                                if not c.valid then return end
                                beautiful.rect_with_corners(cr, width, height, beautiful.border_radius, false, beautiful.border_radius and c.has_border, false, beautiful.border_radius and beautiful.border_radius - beautiful.border_outer_space)
                            end,
                            bg_function = function (context, cr, width, height)
                                return context["client"] == capi.client.focus and beautiful.border_focus or beautiful.border_normal
                            end,
                            fg_function = function (context, cr, width, height)
                                return context["client"] == capi.client.focus and beautiful.fg_focus or beautiful.fg_normal
                            end,
                            widget = cbg
                        },
                        margins = c.has_border and beautiful.border_outer_space or 0,
                        widget = wibox.container.margin
                    },
                    forced_height = beautiful.titlebar_size + (c.has_border and beautiful.border_outer_space * 2 or 0),
                    forced_width = beautiful.mini_titlebar_width + (c.has_border and beautiful.border_outer_space * 2 or 0),
                    widget = wibox.container.constraint
                },
                halign = "right",
                widget = wibox.container.place
            }
                 )
        c:titlebar_bottom(tw_bottom, to_bottom)
    elseif c.has_titlebar and c.titlebar_style == "full_bottom" then
        local tw_bottom = beautiful.titlebar_size + beautiful.border_outer_space
        local to_bottom = 0
        awful.titlebar(c,
                       {
                           position = "bottom",
                           size = tw_bottom,
                           bg = "#00000000",
                           bgimage = c.has_border and draw_tb_border_bgimage_bottom,
                       }
        ) : setup(
            {
                {
                    {
                        {

                            awful.widget.clienticon(c),
                            {
                                image = beautiful.client_default_icon,
                                widget = masked_imagebox,
                            },
                            widget = fallback,
                        },
                        halign = "center",
                        widget = wibox.container.place
                    },
                    shape = function (cr, width, height)
                        if not c.valid then return end
                        beautiful.rect_with_corners(cr, width, height, false, false, beautiful.border_radius and c.has_border, beautiful.border_radius and c.has_border, beautiful.border_radius and beautiful.border_radius - beautiful.border_outer_space)
                    end,
                    bg_function = function (context, cr, width, height)
                        return context["client"] == capi.client.focus and beautiful.border_focus or beautiful.border_normal
                    end,
                    fg_function = function (context, cr, width, height)
                        return context["client"] == capi.client.focus and beautiful.fg_focus or beautiful.fg_normal
                    end,
                    widget = cbg
                },
                margins = c.has_border and beautiful.border_outer_space or 0,
                widget = wibox.container.margin
            }
                 )
        c:titlebar_bottom(tw_bottom, to_bottom)
    elseif c.has_border then
        awful.titlebar(c,
                       {
                           position = "bottom",
                           size = tw,
                           bg = "#00000000",
                           bgimage = draw_tb_border_bgimage_bottom,
                       }
        ) : setup({ widget = wibox.container.background })
        c:titlebar_bottom(tw, to)
    else
        c:titlebar_bottom(0, 0)
    end

    if not c.decoration_created then
        awful.titlebar(c,
                       {
                           position = "left",
                           size = tw,
                           bg = "#00000000",
                           bgimage = draw_tb_border_bgimage_left,
                       }
        ) : setup({ widget = wibox.container.background })
        awful.titlebar(c,
                       {
                           position = "right",
                           size = tw,
                           bg = "#00000000",
                           bgimage = draw_tb_border_bgimage_right,
                       }
        ) : setup({ widget = wibox.container.background })
    end

    if c.has_border then
        c:titlebar_left(tw, to)
        c:titlebar_right(tw, to)
    else
        c:titlebar_left(0, 0)
        c:titlebar_right(0, 0)
    end

    c.decoration_created = true
    delayed_update_shape(c)

    if geo then c:geometry(geo) end
end

capi.client.connect_signal("property::size", delayed_update_shape)
capi.client.connect_signal("request::titlebars", decorate)
capi.client.connect_signal("property::has_border",
                           function (c)
                               if c.previous_has_border ~= nil and c.previous_has_border ~= c.has_border then
                                   decorate(c)
                               end
                               c.previous_has_border = c.has_border
                           end
)
capi.client.connect_signal("property::has_titlebar",
                           function (c)
                               if c.previous_has_titlebar ~= c.has_titlebar then
                                   if shared.var.hide_clients_with_titlebars then
                                       capi.client.emit_signal("list")
                                   end
                                   if c.previous_has_titlebar ~= nil then
                                       decorate(c)
                                   end
                               end
                               c.previous_has_titlebar = c.has_titlebar
                           end
)
capi.client.connect_signal("property::titlebar_style",
                           function (c)
                               if c.previous_titlebar_style ~= nil and c.previous_titlebar_style ~= c.titlebar_style and c.has_titlebar then
                                   decorate(c)
                               end
                               c.previous_titlebar_style = c.titlebar_style
                           end
)

local function reset_decoration(c)
    if c.borderless then
        c.has_border = false
        return
    elseif c.maximized then
        c.has_border = false
    else
        c.has_border = true
    end
    if c.type == "dock" then
        c.has_titlebar = false
    else
        if false -- c.maximized and not shared.var.enable_titlebar
        then
            c.has_titlebar = false
        else
            c.has_titlebar = true
        end
    end
end

local function manage_cb(c)
    c.has_titlebar_enabled = shared.var.enable_titlebar
    reset_decoration(c)
    decorate(c)

    local bw = beautiful.border_width

    -- c:titlebar_left(bw * 2, bw)
    -- c:titlebar_right(bw * 2, bw)
    -- c:titlebar_top(bw * 2, bw)
    -- c:titlebar_bottom(bw * 2, bw)

    -- c:connect_signal("property::size", delayed_update_shape)
    -- delayed_update_shape(c)
end

capi.client.connect_signal("manage", manage_cb)
capi.client.connect_signal("property::maximized", reset_decoration)

-- rules

function placement_skip_existing(placement)
    return function(c, args)
        if c.focus_timestamp ~= nil then return nil end
        return placement(c, args)
    end
end

awful.placement.centered_on_new = placement_skip_existing(awful.placement.centered)
awful.placement.centered_with_half_size_on_new = placement_skip_existing(
    function (c, args)
        local sgeo = c.screen.workarea
        local geo = {x = sgeo.x + sgeo.width / 4, y = sgeo.y + sgeo.height / 4, width = sgeo.width / 2, height = sgeo.height / 2}
        c:geometry(geo)
        return geo
    end
)

require("awful.rules").rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = true,
         keys = client_keys,
         buttons = client_buttons,
         border_width = 0,
         screen = function(c) return capi.awesome.startup and c.screen or awful.screen.focused() end,
         floating = shared.var.floating_by_default,
         placement = awful.placement.centered_on_new,
         titlebar_style = "mini",
      }
   },
   {
      rule = { class = "Synapse" },
      properties = {
         ontop = true,
      },
   },
   {
      rule = { class = "Firefox" },
      properties = {
         titlebar_style = "mini_bottom",
      },
   },

   {
       rule = { class = "Rofi" },
       properties = {
           placement = awful.placement.centered,
           skip_taskbar = true,
           callback = function (c)
               c:connect_signal("unfocus", function() c:kill() end)
           end,
       },
   },
   {
      rule = { type = "dock" },
      properties = {
         floating = true,
         sticky = true,
         ontop = true,
         focusable = false,
         below = false,
         has_client_input_shape = true,
         borderless = true,
      },
   },
   {
      rule = { class = "Conky" },
      properties = {
         floating = true,
         sticky = true,
         ontop = false,
         below = true,
         focus = false,
         borderless = true,
         focusable = false,
      }
   },
   {
       rule = { class = "Xfdesktop" },
       except = { type = "dialog" },
       properties = {
           borderless = true,
           sticky = true,
           fullscreen = false,
       },
   },
   {
       rule = { class = "tabbed" },
       properties = {
           placement = awful.placement.centered_with_half_size_on_new,
       },
   },
   {
       rule = { class = "mpv" },
       properties = {
           size_hints_honor = false,
       },
   },
}

return nil

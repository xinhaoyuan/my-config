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
local extender = require("extender")
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
   if not c.has_xtitlebar_enabled then
      shared.client.titlebar_enable(c)
   else
      shared.client.titlebar_disable(c)
   end
end

function shared.client.titlebar_enable(c)
   c.has_xtitlebar_enabled = true
   shared.client.titlebar_show(c)
end

function shared.client.titlebar_disable(c)
   c.has_xtitlebar_enabled = false
   shared.client.titlebar_hide(c)
end

function shared.client.titlebar_show(c)
    c.has_xtitlebar = true
end

function shared.client.titlebar_hide(c)
    c.has_xtitlebar = false
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

local all_titlebar_styles = {"full_top", "mini_top", "mini_mid", "mini_bottom", "full_bottom"}
function shared.client.cycle_titlebar_style(c, step)
    for i, s in ipairs(all_titlebar_styles) do
        if s == c.titlebar_style then
            local new_index = (i + step - 1) % #all_titlebar_styles + 1
            c.titlebar_style = all_titlebar_styles[new_index]
            print("Cycle from", i, "to", new_index)
            return
        end
    end
    print("Cannot cycle from style", c.titlebar_style)
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
    if c and c.type == "desktop" then c = nil end
    if c then
        c.maximized = false
        c.maximized_vertical = false
        c.maximized_horizontal = false
        c.fullscreen = false
        c.floating = false
        c:raise()
    end
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
    awful.key({ "Mod4" }, "Prior", shared.client.enlarge),
    awful.key({ "Mod4" }, "Next", shared.client.shrink),
    awful.key({ "Mod4" }, "=", function (c) machi.default_editor.adjust_x_shares(c, 50) end),
    awful.key({ "Mod4" }, "-", function (c) machi.default_editor.adjust_x_shares(c, -50) end),
    awful.key({ "Mod4", "Shift" }, "=", function (c) machi.default_editor.adjust_y_shares(c, 50) end),
    awful.key({ "Mod4", "Shift" }, "-", function (c) machi.default_editor.adjust_y_shares(c, -50) end),

    awful.key({ "Mod4" }, "q", function (c)
            local other_clients = {}
            for _, oc in ipairs(c.screen.tiled_clients) do
                if oc ~= c then
                    table.insert(other_clients, oc)
                end
            end
            local geo = extender.fit(
                c.screen.workarea, other_clients,
                {
                    min_size = beautiful.useless_gap * 4,
                }
            )
            if geo then
                c.maximized = false
                c.maximized_vertical = false
                c.maximized_horizontal = false
                c.fullscreen = false
                c.floating = false
                c:geometry(geo)
            end
    end),

    awful.key({ "Mod4" }, "'", function (c)
            shared.client.cycle_titlebar_style(c, 1)
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

--     if shared.vars.titlebar_position == "left" or shared.vars.titlebar_position == "right" then
--         titlebar_container = wibox.container.rotate(titlebar_container, "west")
--     end

--     titlebar_container = wibox.widget {
--         titlebar_container,
--         [opposite_dir[shared.vars.titlebar_position]] = beautiful.xborder_width,
--         color = capi.client.focus == c and beautiful.border_focus or beautiful.border_normal,
--         widget = wibox.container.margin,
--     }
--     awful.titlebar(
--         c,
--         {
--             position = shared.vars.titlebar_position,
--             size = beautiful.titlebar_size + beautiful.xborder_width,
--             font = beautiful.font,
--         }
--     ):setup({ titlebar_container, widget = wibox.container.margin })
--     c.titlebar_container = titlebar_container
-- end

local border_top = border.directions{ "top", "left", "right" }
local function draw_tb_border_bgimage_top(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    local indicator = beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width and beautiful.xborder_radius_cut and beautiful.xborder_indicator and capi.client.focus == c
    if beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width then
        cr:set_source(gcolor(beautiful.xborder_space))
        if indicator then
            gshape.rectangle(cr, width, height)
        else
            beautiful.rect_with_corners(cr, width, height + beautiful.xborder_radius, true, true, false, false, beautiful.xborder_radius)
        end
        cr:fill()
        cr:set_operator('ATOP')
    end
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_top)
    if indicator then
        cr:set_source(gcolor(beautiful.fg_normal))
        local indent = (2 - math.sqrt(2)) * (beautiful.xborder_radius) - beautiful.xborder_outer_space

        cr:move_to(beautiful.xborder_outer_space, beautiful.xborder_outer_space)
        cr:line_to(beautiful.xborder_outer_space, indent)
        cr:line_to(indent, beautiful.xborder_outer_space)
        cr:fill()

        cr:move_to(width - indent, beautiful.xborder_outer_space)
        cr:line_to(width - beautiful.xborder_outer_space, beautiful.xborder_outer_space)
        cr:line_to(width - beautiful.xborder_outer_space, indent)
        cr:fill()
    end
end


local border_bottom = border.directions{ "bottom", "left", "right" }
local function draw_tb_border_bgimage_bottom(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    local indicator = beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width and beautiful.xborder_radius_cut and beautiful.xborder_indicator and capi.client.focus == c
    if beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width then
        cr:set_source(gcolor(beautiful.xborder_space))
        if indicator then
            gshape.rectangle(cr, width, height)
        else
            cr:translate(0, -beautiful.xborder_radius)
            beautiful.rect_with_corners(cr, width, height + beautiful.xborder_radius, false, false, true, true, beautiful.xborder_radius)
            cr:translate(0, beautiful.xborder_radius)
        end
        cr:fill()
        cr:set_operator('ATOP')
    end
    border:draw({ theme = beautiful.get_border_theme(), color = border_color }, cr, width, height, border_bottom)
    if indicator then
        cr:set_source(gcolor(beautiful.fg_normal))
        local indent = (2 - math.sqrt(2)) * (beautiful.xborder_radius) - beautiful.xborder_outer_space

        cr:move_to(width - indent, height - beautiful.xborder_outer_space)
        cr:line_to(width - beautiful.xborder_outer_space, height - beautiful.xborder_outer_space)
        cr:line_to(width - beautiful.xborder_outer_space, height - indent)
        cr:fill()

        cr:move_to(indent, height - beautiful.xborder_outer_space)
        cr:line_to(beautiful.xborder_outer_space, height - beautiful.xborder_outer_space)
        cr:line_to(beautiful.xborder_outer_space, height - indent)
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
    if c.fullscreen then
        -- Awesome handles fullscreen window differently. Assume no border or titlebar.
        apply_container_shape(c, nil, nil)
        return
    end
    local mini_titlebar_shape
    local padding = (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space)
    local radius = beautiful.xborder_radius or 0
    if not c.has_xtitlebar then
        mini_titlebar_shape = nil
    elseif c.titlebar_style == "mini_top" then
        mini_titlebar_shape = function (cr, width, height)
            cr:save()
            cr:translate(width - beautiful.mini_titlebar_width - padding - beautiful.xborder_indent * 2, 0)
            beautiful.rect_with_corners(cr,
                                        beautiful.mini_titlebar_width + padding + beautiful.xborder_indent * 2,
                                        beautiful.mini_titlebar_size + padding,
                                        false, false, false, radius - beautiful.xborder_width + beautiful.xborder_inner_space * 2)
            cr:restore()
        end
    elseif c.titlebar_style == "mini_mid" then
        mini_titlebar_shape = function (cr, width, height)
            cr:save()
            local bar_overall_height = beautiful.mini_titlebar_width + beautiful.xborder_inner_space * 2 + beautiful.xborder_indent * 2
            cr:translate(width - beautiful.mini_titlebar_size - padding, (height - bar_overall_height) / 2)
            beautiful.rect_with_corners(cr,
                                        beautiful.mini_titlebar_size + padding,
                                        bar_overall_height,
                                        true,
                                        false,
                                        false,
                                        true,
                                        radius - beautiful.xborder_width + beautiful.xborder_inner_space * 2)
            cr:restore()
        end
    elseif c.titlebar_style == "mini_bottom" then
        mini_titlebar_shape = function (cr, width, height)
            cr:save()
            cr:translate(width - beautiful.mini_titlebar_width - padding - beautiful.xborder_indent * 2, height - beautiful.mini_titlebar_size - padding)
            beautiful.rect_with_corners(cr,
                                        beautiful.mini_titlebar_width + padding + beautiful.xborder_indent * 2,
                                        beautiful.mini_titlebar_size + padding,
                                        radius - beautiful.xborder_width + beautiful.xborder_inner_space * 2, false, false, false)
            cr:restore()
        end
    end
    apply_container_shape(
        c,
        radius > 0 and c.has_xborder and function (cr, width, height)
            cr:save()
            cr:translate(beautiful.xborder_width, beautiful.xborder_width)
            beautiful.rect_with_corners(cr,
                                        width - beautiful.xborder_width * 2,
                                        height - beautiful.xborder_width * 2,
                                        true, true, true, true,
                                        radius - beautiful.xborder_width)
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

local function get_mini_titlebar(c)
    if c.mini_titlebar == nil then
        c.mini_titlebar = wibox.widget {
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
                    margins = (beautiful.mini_titlebar_size - beautiful.bar_icon_size) / 2,
                    widget = wibox.container.margin,
                },
                halign = "center",
                valign = "center",
                widget = wibox.container.background,
            },
            forced_height = beautiful.mini_titlebar_size,
            forced_width = beautiful.mini_titlebar_width,
            widget = wibox.container.constraint
        }
    end
    return c.mini_titlebar
end

local function mini_titlebar_button(c, w, button)
    if button == 1 then
        if w.client_was_focused_when_pressed then
            c.maximized = not c.maximized
        end
    elseif button == 2 then
        c:kill()
    elseif button == 3 then
        shared.waffle.show_client_waffle(c, { anchor = "mouse" })
    elseif button == 4 then
        shared.client.cycle_titlebar_style(c, -1)
    elseif button == 5 then
        shared.client.cycle_titlebar_style(c, 1)
    end
end

local function set_up_mini_titlebar_buttons(c, w)
    w.button_pressed = {}
    w:connect_signal('button::press', function (w, x, y, button)
                         w.button_pressed[button] = true
                         w.client_was_focused_when_pressed = capi.client.focus == c
    end)
    w:connect_signal('button::release', function (w, x, y, button)
                         if w.button_pressed[button] then
                             w.button_pressed[button] = false
                             mini_titlebar_button(c, w, button)
                         end
    end)
    w:connect_signal('mouse::leave', function (w, x, y, button)
                         if w.button_pressed[1] then
                             c.maximized = false
                             c.minimized = false
                             awful.mouse.client.move(c)
                         elseif w.button_pressed[3] then
                             c.maximized = false
                             c.minimized = false
                             local _, cc = awful.placement.closest_corner(capi.mouse, {parent = c})
                             awful.mouse.client.resize(c, cc)
                         end
                         w.button_pressed = {}
    end)
end

local function get_full_titlebar(c)
    if c.full_titlebar == nil then
        c.full_titlebar = wibox.widget {
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
                        margins = (beautiful.bar_height - beautiful.bar_icon_size) / 2,
                        widget = wibox.container.margin,
                    },
                    valign = "center",
                    widget = wibox.container.place
                },
                { -- Title
                    align  = 'center',
                    widget = awful.titlebar.widget.titlewidget(c)
                },
                spacing = beautiful.sep_small_size,
                layout = wibox.layout.fixed.horizontal,
            },
            halign = "center",
            widget = wibox.container.place
        }
    end
    return c.full_titlebar
end

local function set_up_full_titlebar_buttons(c, w)
    -- Currently the titlebar widget is reused, so avoid setting multiple callbacks here.
    if w._has_full_titlebar_buttons then
        return
    end
    w._has_full_titlebar_buttons = true

    w:connect_signal('button::press', function (w, x, y, button)
                         if button == 1 then
                             c:raise()
                             c.maximized = false
                             c.minimized = false
                             awful.mouse.client.move(c)
                         end
    end)
    w:connect_signal('button::release', function (w, x, y, button)
                         if button == 3 then
                             shared.waffle.show_client_waffle(c, { anchor = "mouse" })
                         elseif button == 4 then
                             shared.client.cycle_titlebar_style(c, -1)
                         elseif button == 5 then
                             shared.client.cycle_titlebar_style(c, 1)
                         end
    end)
end

-- Note that the awesome native titlebars ared used for both borders and titlebar here.
local function decorate(c)
    if not c.valid then return end
    local geo = c:geometry()
    local restore_geo = c.decoration_created

    local tw
    local to

    local has_radius = beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width

    if has_radius  then
        tw = beautiful.xborder_radius
        to = tw - beautiful.xborder_width
    else
        tw = beautiful.xborder_width
        to = 0
    end

    if c.has_xtitlebar and c.titlebar_style == "mini_top" then
        local tw_top = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space)
        local to_top = tw_top - (c.has_xborder and beautiful.xborder_width or 0)
        local widget = wibox.widget {
            {
                {
                    get_mini_titlebar(c),
                    right = beautiful.xborder_indent,
                    widget = wibox.container.margin
                },
                halign = "right",
                widget = wibox.container.place
            },
            shape = function (cr, width, height)
                if not c.valid then return end
                beautiful.rect_with_corners(cr, width, height,
                                            false,
                                            has_radius and c.has_xborder,
                                            false,
                                            has_radius and true,
                                            has_radius and beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
            end,
            bg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_bg_focus or beautiful.titlebar_bg_normal
            end,
            fg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_fg_focus or beautiful.titlebar_fg_normal
            end,
            widget = cbg
        }
        set_up_mini_titlebar_buttons(c, widget)
        awful.titlebar(c,
                       {
                           position = "top",
                           size = tw_top,
                           bg = c.has_xborder and "#00000000" or beautiful.xborder_space,
                           bgimage = c.has_xborder and draw_tb_border_bgimage_top,
                       }
        ) : setup({
            {
                {
                    widget,
                    top = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    right = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    left = beautiful.xborder_inner_space,
                    bottom = beautiful.xborder_inner_space,
                    widget = wibox.container.margin
                },
                forced_height = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space),
                forced_width = beautiful.mini_titlebar_width + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space) + beautiful.xborder_indent * 2,
                -- Eliminate the artifact on the boundary
                bgimage = function (context, cr, width, height)
                    if not c.valid or not c.has_xborder or not has_radius then return end
                    cr:translate(beautiful.xborder_inner_space + 1,
                                 beautiful.xborder_width - beautiful.xborder_inner_space - 1)
                    beautiful.rect_with_corners(cr,
                                                beautiful.mini_titlebar_width + beautiful.xborder_indent * 2,
                                                beautiful.mini_titlebar_size,
                                                false,
                                                true,
                                                false,
                                                true,
                                                has_radius and beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                    cr:set_source(gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal))
                    cr:fill()
                end,
                widget = wibox.container.background,
            },
            halign = "right",
            widget = wibox.container.place
        })
        c:titlebar_top(tw_top, to_top)
    elseif c.has_xtitlebar and c.titlebar_style == "full_top" then
        local tw_top = beautiful.titlebar_size + (c.has_xborder and beautiful.xborder_width or 0)
        local to_top = 0
        local widget = get_full_titlebar(c)
        set_up_full_titlebar_buttons(c, widget)
        awful.titlebar(c,
                       {
                           position = "top",
                           size = tw_top,
                           bg = "#00000000",
                           bgimage = c.has_xborder and draw_tb_border_bgimage_top,
                       }
        ) : setup(
            {
                {
                    {
                        widget,
                        shape = function (cr, width, height)
                            if not c.valid then return end
                            beautiful.rect_with_corners(cr, width, height, beautiful.xborder_radius and c.has_xborder, beautiful.xborder_radius and c.has_xborder, false, false, beautiful.xborder_radius and beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                        end,
                        bg_function = function (context, cr, width, height)
                            return context["client"] == capi.client.focus and beautiful.titlebar_bg_focus or beautiful.titlebar_bg_normal
                        end,
                        fg_function = function (context, cr, width, height)
                            return context["client"] == capi.client.focus and beautiful.titlebar_fg_focus or beautiful.titlebar_fg_normal
                        end,
                        widget = cbg
                    },
                    top = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    left = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    right = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    bottom = c.has_xborder and beautiful.xborder_inner_space or 0,
                    widget = wibox.container.margin
                },
                -- Eliminate the artifact on the boundary
                bgimage = function (context, cr, width, height)
                    if not c.valid or not c.has_xborder or not has_radius then return end
                    cr:translate(beautiful.xborder_width - beautiful.xborder_inner_space,
                                 beautiful.xborder_width - beautiful.xborder_inner_space - 1)
                    beautiful.rect_with_corners(cr,
                                                width - (beautiful.xborder_width - beautiful.xborder_inner_space) * 2,
                                                beautiful.titlebar_size,
                                                true,
                                                true,
                                                false,
                                                false,
                                                beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                    cr:set_source(gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal))
                    cr:fill()
                end,
                widget = wibox.container.background,
            }
                 )
        c:titlebar_top(tw_top, to_top)
    elseif c.has_xborder then
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

    if c.has_xtitlebar and c.titlebar_style == "mini_bottom" then
        local tw_bottom = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space)
        local to_bottom = tw_bottom - (c.has_xborder and beautiful.xborder_width or 0)
        local widget = wibox.widget {
            {
                {
                    get_mini_titlebar(c),
                    right = beautiful.xborder_indent,
                    widget = wibox.container.margin
                },
                halign = "right",
                widget = wibox.container.place
            },
            shape = function (cr, width, height)
                if not c.valid then return end
                beautiful.rect_with_corners(cr, width, height,
                                            has_radius and true,
                                            false,
                                            has_radius and c.has_xborder,
                                            false,
                                            beautiful.xborder_radius and beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
            end,
            bg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_bg_focus or beautiful.titlebar_bg_normal
            end,
            fg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_fg_focus or beautiful.titlebar_fg_normal
            end,
            widget = cbg
        }
        set_up_mini_titlebar_buttons(c, widget)
        awful.titlebar(c,
                       {
                           position = "bottom",
                           size = tw_bottom,
                           bg = c.has_xborder and "#00000000" or beautiful.xborder_space,
                           bgimage = c.has_xborder and draw_tb_border_bgimage_bottom,
                       }
        ) : setup(
            {
                {
                    {
                        widget,
                        top = beautiful.xborder_inner_space,
                        left = beautiful.xborder_inner_space,
                        right = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                        bottom = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                        widget = wibox.container.margin
                    },
                    forced_height = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space),
                    forced_width = beautiful.mini_titlebar_width + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space) + beautiful.xborder_indent * 2,
                    -- Eliminate the artifact on the boundary
                    bgimage = function (context, cr, width, height)
                        if not c.valid or not c.has_xborder or not has_radius then return end
                        cr:translate(beautiful.xborder_inner_space + 1,
                                     beautiful.xborder_inner_space + 1)
                        beautiful.rect_with_corners(cr,
                                                    beautiful.mini_titlebar_width + beautiful.xborder_indent * 2,
                                                    beautiful.mini_titlebar_size,
                                                    true,
                                                    false,
                                                    true,
                                                    false,
                                                    beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                        cr:set_source(gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal))
                        cr:fill()
                    end,
                    widget = wibox.container.background,
                },
                halign = "right",
                widget = wibox.container.place
            }
                 )
        c:titlebar_bottom(tw_bottom, to_bottom)
    elseif c.has_xtitlebar and c.titlebar_style == "full_bottom" then
        local tw_bottom = beautiful.titlebar_size + (c.has_xborder and beautiful.xborder_width or 0)
        local to_bottom = 0
        local widget = get_full_titlebar(c)
        set_up_full_titlebar_buttons(c, widget)
        awful.titlebar(c,
                       {
                           position = "bottom",
                           size = tw_bottom,
                           bg = "#00000000",
                           bgimage = c.has_xborder and draw_tb_border_bgimage_bottom,
                       }
        ) : setup(
            {
                {
                    {
                        widget,
                        shape = function (cr, width, height)
                            if not c.valid then return end
                            beautiful.rect_with_corners(cr, width, height, false, false, beautiful.xborder_radius and c.has_xborder, beautiful.xborder_radius and c.has_xborder, beautiful.xborder_radius and beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                        end,
                        bg_function = function (context, cr, width, height)
                            return context["client"] == capi.client.focus and beautiful.titlebar_bg_focus or beautiful.titlebar_bg_normal
                        end,
                        fg_function = function (context, cr, width, height)
                            return context["client"] == capi.client.focus and beautiful.titlebar_fg_focus or beautiful.titlebar_fg_normal
                        end,
                        widget = cbg
                    },
                    top = c.has_xborder and beautiful.xborder_inner_space or 0,
                    left = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    right = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    bottom = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                    widget = wibox.container.margin
                },
                -- Eliminate the artifact on the boundary
                bgimage = function (context, cr, width, height)
                    if not c.valid or not c.has_xborder or not has_radius then return end
                    cr:translate(beautiful.xborder_inner_space,
                                 beautiful.xborder_width - beautiful.xborder_inner_space + 1)
                    beautiful.rect_with_corners(cr,
                                                width - (beautiful.xborder_width - beautiful.xborder_inner_space) * 2,
                                                beautiful.titlebar_size,
                                                false,
                                                false,
                                                true,
                                                true,
                                                beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
                    cr:set_source(gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal))
                    cr:fill()
                end,
                widget = wibox.container.background,
            }
                 )
        c:titlebar_bottom(tw_bottom, to_bottom)
    elseif c.has_xborder then
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

    if c.has_xtitlebar and c.titlebar_style == "mini_mid" then
        local tw_right = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space)
        local to_right = tw_right - (c.has_xborder and beautiful.xborder_width or 0)
        local widget = wibox.widget {
            {
                {
                    get_mini_titlebar(c),
                    direction = "west",
                    widget = wibox.container.rotate
                },
                top = beautiful.xborder_indent,
                bottom = beautiful.xborder_indent,
                widget = wibox.container.margin,
            },
            shape = function (cr, width, height)
                if not c.valid then return end
                beautiful.rect_with_corners(cr, width, height,
                                            has_radius,
                                            false,
                                            false,
                                            has_radius,
                                            beautiful.xborder_radius - beautiful.xborder_width + beautiful.xborder_inner_space)
            end,
            bg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_bg_focus or beautiful.titlebar_bg_normal
            end,
            fg_function = function (context, cr, width, height)
                return context["client"] == capi.client.focus and beautiful.titlebar_fg_focus or beautiful.titlebar_fg_normal
            end,
            widget = cbg
        }
        set_up_mini_titlebar_buttons(c, widget)
        awful.titlebar(c,
                       {
                           position = "right",
                           size = tw_right,
                           bg = c.has_xborder and "#00000000" or beautiful.xborder_space,
                           bgimage = c.has_xborder and draw_tb_border_bgimage_right,
                       }
        ) : setup(
            {
                {
                    {
                        widget,
                        top = beautiful.xborder_inner_space,
                        left = beautiful.xborder_inner_space,
                        right = c.has_xborder and beautiful.xborder_width - beautiful.xborder_inner_space or 0,
                        bottom = beautiful.xborder_inner_space,
                        widget = wibox.container.margin
                    },
                    forced_width = beautiful.mini_titlebar_size + (c.has_xborder and beautiful.xborder_width or beautiful.xborder_inner_space),
                    forced_height = beautiful.mini_titlebar_width + beautiful.xborder_inner_space * 2 + beautiful.xborder_indent * 2,
                    widget = wibox.container.constraint
                },
                valign = "center",
                widget = wibox.container.place
            }
                 )
        c:titlebar_right(tw_right, to_right)
    elseif c.has_xborder then
        awful.titlebar(c,
                       {
                           position = "right",
                           size = tw,
                           bg = "#00000000",
                           bgimage = draw_tb_border_bgimage_right,
                       }
        ) : setup({ widget = wibox.container.background })
        c:titlebar_right(tw, to)
    else
        c:titlebar_right(0, 0)
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
    end

    if c.has_xborder then
        c:titlebar_left(tw, to)
    else
        c:titlebar_left(0, 0)
    end

    c.decoration_created = true
    delayed_update_shape(c)

    if restore_geo then c:geometry(geo) end
end

capi.client.connect_signal("property::size", delayed_update_shape)
capi.client.connect_signal("request::titlebars", decorate)
capi.client.connect_signal("property::has_xborder",
                           function (c)
                               if c.previous_has_xborder ~= nil and c.previous_has_xborder ~= c.has_xborder then
                                   decorate(c)
                               end
                               c.previous_has_xborder = c.has_xborder
                           end
)
capi.client.connect_signal("property::has_xtitlebar",
                           function (c)
                               if c.previous_has_xtitlebar ~= c.has_xtitlebar then
                                   if shared.vars.hide_clients_with_titlebars then
                                       capi.client.emit_signal("list")
                                   end
                                   if c.previous_has_xtitlebar ~= nil then
                                       decorate(c)
                                   end
                               end
                               c.previous_has_xtitlebar = c.has_xtitlebar
                           end
)
capi.client.connect_signal("property::titlebar_style",
                           function (c)
                               if c.previous_titlebar_style ~= nil and c.previous_titlebar_style ~= c.titlebar_style and c.has_xtitlebar then
                                   decorate(c)
                               end
                               c.previous_titlebar_style = c.titlebar_style
                           end
)

local function reset_decoration(c)
    if c.borderless then
        c.has_xborder = false
    elseif c.maximized then
        c.has_xborder = false
    else
        c.has_xborder = true
    end
    if c.type == "dock" or c.type == "desktop" then
        c.has_xtitlebar = false
    elseif c.has_xtitlebar == nil then
        if c.requests_no_titlebar and c.respect_titlebar_request then
            c.has_xtitlebar = false
        else
            c.has_xtitlebar = true
        end
    end
end

local function manage_cb(c)
    local geo = c:geometry()
    c.has_xtitlebar_enabled = shared.vars.enable_titlebar
    reset_decoration(c)
    decorate(c)

    local bw = beautiful.xborder_width

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

function awful.placement.fullscreen(c, args)
    c:geometry(c.screen.geometry)
end

local function placement_skip_existing(placement)
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
awful.placement.centered_with_full_size_on_new = placement_skip_existing(
    function (c, args)
        local sgeo = c.screen.workarea
        local geo = {x = sgeo.x, y = sgeo.y, width = sgeo.width, height = sgeo.height}
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
         floating = shared.vars.floating_by_default,
         placement = awful.placement.centered_on_new,
         titlebar_style = "mini_top",
      }
   },
   {
      rule = { class = "Synapse" },
      properties = {
         ontop = true,
      },
   },
   {
       rule = { class = "Rofi" },
       properties = {
           placement = awful.placement.centered_on_new,
           skip_taskbar = true,
           respect_titlebar_request = true,
           callback = function (c)
               c:connect_signal("unfocus", function() c:kill() end)
           end,
       },
   },
   {
       rule = { class = "Firefox", role = "Popup" },
       properties = {
           borderless = true,
           has_xtitlebar = false,
       },
   },
   {
       rule = { type = "dialog" },
       properties = {
           titlebar_style = "full_top",
           callback = function (c)
               if c.transient_for and c.transient_for.type == "desktop" then
                   c.above = true
               end
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
       rule = { type = "desktop" },
       properties = {
           borderless = true,
           sticky = true,
           fullscreen = false,
           placement = awful.placement.fullscreen,
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
   {
       rule = { class = "Steam" },
       properties = {
           borderless = true,
           callback = function (c)
               -- c:deny("geometry", "ewmh")
           end,
       },
   },
   {
       rule = { class = "discord" },
       properties = {
           size_hints_honor = false,
       },
   },
   {
       rule = { class = "TelegramDesktop" },
       properties = {
           size_hints_honor = false,
       },
   },
}

return nil

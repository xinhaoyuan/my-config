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
    if not c.has_titlebar then
        local geo
        if c.titlebar_created then geo = c:geometry() end
        for _, d in ipairs({"top", "bottom", "left", "right"}) do
            awful.titlebar.show(c, d)
        end
        if geo then c:geometry(geo) end
        c.has_titlebar = true
        if shared.var.hide_clients_with_titlebars then
            capi.client.emit_signal("list")
        end
    end
end

function shared.client.titlebar_hide(c)
    if c.has_titlebar then
        local geo
        if c.titlebar_created then geo = c:geometry() end
        for _, d in ipairs({"top", "bottom", "left", "right"}) do
            awful.titlebar.hide(c, d)
        end
        if geo then c:geometry(geo) end
        c.has_titlebar = false
        if shared.var.hide_clients_with_titlebars then
            capi.client.emit_signal("list")
        end
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

    awful.key({ "Mod4" }, "t", function (c) shared.client.titlebar_toggle(c) end),

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

local border_theme
if beautiful.border_radius == nil then
    border_theme = border.default_theme
else
    border_theme = setmetatable({}, {__index = border.rounded_theme})
    border_theme:init()
    border_theme.size = beautiful.border_radius
    border_theme.outer_space = beautiful.border_outer_space
    border_theme.inner_space = beautiful.border_radius - beautiful.border_width + beautiful.border_inner_space
end

local border_top = border.directions{ "top", "left", "right" }
local function draw_tb_border_bgimage_top(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    if beautiful.border_radius then
        gshape.partially_rounded_rect(cr, width, height + beautiful.border_radius, true, true, false, false, beautiful.border_radius)
        cr:fill()
        cr:set_operator('ATOP')
    end
    border:draw({ theme = border_theme, color = border_color }, cr, width, height, border_top)
end


local border_bottom = border.directions{ "bottom", "left", "right" }
local function draw_tb_border_bgimage_bottom(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    if beautiful.border_radius then
        cr:translate(0, -beautiful.border_radius)
        gshape.partially_rounded_rect(cr, width, height + beautiful.border_radius, false, false, true, true, beautiful.border_radius)
        cr:fill()
        cr:set_operator('ATOP')
        cr:translate(0, beautiful.border_radius)
    end
    border:draw({ theme = border_theme, color = border_color }, cr, width, height, border_bottom)
end

local border_left = border.directions{ "left" }
local function draw_tb_border_bgimage_left(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ theme = border_theme, color = border_color }, cr, width, height, border_left)
end

local border_right = border.directions{ "right" }
local function draw_tb_border_bgimage_right(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ theme = border_theme, color = border_color }, cr, width, height, border_right)
end

local function apply_container_shape(client, shape, ...)
  local geo = client:geometry()

  local img = cairo.ImageSurface(cairo.Format.A1, geo.width, geo.height)
  local cr = cairo.Context(img)

  cr:set_operator(cairo.Operator.CLEAR)
  cr:set_source_rgba(0,0,0,1)
  cr:paint()
  cr:set_operator(cairo.Operator.SOURCE)
  cr:set_source_rgba(1,1,1,1)

  shape(cr, geo.width, geo.height, ...)

  cr:fill()

  client.client_container_shape = img._native
  img:finish()
end

local function my_client_shape(cr, width, height)
    gshape.rounded_rect(cr, width, height, beautiful.border_radius)
end

local function update_shape(c)
    if beautiful.border_radius == nil then
        return
    end

    if c.maximized or c.fullscreen then
        c.client_container_shape = nil
        if c._shape ~= nil then
            c.shape = nil
        end
    else
        -- if c._shape ~= my_client_shape then
        --     c.shape = my_client_shape
        -- end
        apply_container_shape(
            c,
            function (cr, width, height)
                cr:translate(beautiful.border_width, beautiful.border_width)
                gshape.rounded_rect(cr,
                                    width - beautiful.border_width * 2,
                                    height - beautiful.border_width * 2,
                                    beautiful.border_radius - beautiful.border_width)
            end
        )
    end
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

local function create_titlebars(c)
    local tw
    local to
    if beautiful.border_radius == nil then
        tw = beautiful.border_width
        to = 0
    else
        tw = beautiful.border_radius
        to = beautiful.border_radius - beautiful.border_width
    end
    awful.titlebar(c,
                   {
                       position = "top",
                       size = tw,
                       bg = "#00000000",
                       bgimage = draw_tb_border_bgimage_top,
                   }
    ) : setup({ widget = wibox.container.background })
    awful.titlebar(c,
                   {
                       position = "bottom",
                       size = tw,
                       bg = "#00000000",
                       bgimage = draw_tb_border_bgimage_bottom,
                   }
    ) : setup({ widget = wibox.container.background })
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


    if to > 0 then
        c:titlebar_left(tw, to)
        c:titlebar_right(tw, to)
        c:titlebar_top(tw, to)
        c:titlebar_bottom(tw, to)
    end

    c.titlebar_created = true
    c:connect_signal("property::size", delayed_update_shape)
    delayed_update_shape(c)
end

capi.client.connect_signal("request::titlebars", create_titlebars)

local function reset_decoration(c)
    if c.borderless then
        c.border_width = 0
        return
    elseif c.maximized then
        c.border_width = 0
    else
        c.border_width = 0
    end
    if c.type ~= "dock" then
        if c.maximized -- and not shared.var.enable_titlebar
        then
            shared.client.titlebar_hide(c)
        else
            if c.has_titlebar_enabled then
                shared.client.titlebar_show(c)
            end
        end
    end
end

local function manage_cb(c)
    c.has_titlebar = false
    c.has_titlebar_enabled = shared.var.enable_titlebar
    reset_decoration(c)

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

require("awful.rules").rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = true,
         keys = client_keys,
         buttons = client_buttons,
         border_color = beautiful.border_normal,
         screen = function(c) return capi.awesome.startup and c.screen or awful.screen.focused() end,
         floating = shared.var.floating_by_default,
         placement = awful.placement.centered,
         border_width = 0,
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
           callback = function(c)
               local screen_geo = c.screen.geometry
               c:geometry({
                       x = screen_geo.x + screen_geo.width / 4,
                       y = screen_geo.y + screen_geo.height / 4,
                       width = screen_geo.width / 2,
                       height = screen_geo.height / 2
               })
           end
       }
   },
   {
       rule = { class = "mpv" },
       properties = {
           size_hints_honor = false,
       },
   },
}

return nil

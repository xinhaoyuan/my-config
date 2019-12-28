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
local machi = require("layout-machi")
local border = require("border-theme")

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
    local geo = c:geometry()
    for _, d in ipairs({"top", "bottom", "left", "right"}) do
        awful.titlebar.show(c, d)
    end
    if not c.has_titlebar then
        c.has_titlebar = true
        if shared.var.hide_clients_with_titlebars then
            capi.client.emit_signal("list")
        end
    end
    -- c:geometry(geo)
end

function shared.client.titlebar_hide(c)
    local geo = c:geometry()
    for _, d in ipairs({"top", "bottom", "left", "right"}) do
        awful.titlebar.hide(c, d)
    end
    if c.has_titlebar then
        c.has_titlebar = false
        if shared.var.hide_clients_with_titlebars then
            capi.client.emit_signal("list")
        end
    end
    c:geometry(geo)
end

local table_join = awful.util.table.join
local delayed = gtimer.delayed_call

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

local client_keys = table_join(
   awful.key({ "Mod4" }, ".", function (c)
         c.maximized = false
         c.maximized_vertical = false
         c.maximized_horizontal = false
         c.fullscreen = false
         c.floating = false
         c:raise()
         delayed(
            function ()
               machi.switcher.start(c)
            end
         )
   end),

   awful.key({ "Mod4" }, "Tab", function (c)
         c.maximized = false
         c.maximized_vertical = false
         c.maximized_horizontal = false
         c.fullscreen = false
         c.floating = false
         c:raise()
         delayed(
            function ()
               machi.switcher.start(c, {["Super_L"] = true})
            end
         )
   end),

   awful.key({ "Mod4" }, "Prior", shared.client.enlarge),
   awful.key({ "Mod4" }, "Next", shared.client.shrink),
   awful.key({ "Mod4" }, "=", function (c) c.ontop = not c.ontop end),
   awful.key({ "Mod4" }, "-", function (c) c.sticky = not c.sticky end),

   awful.key({ "Mod4" }, "t", function (c) shared.client.titlebar_toggle(c) end),

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
    border:draw({ color = border_color }, cr, width, height, border_top)
end


local border_bottom = border.directions{ "bottom", "left", "right" }
local function draw_tb_border_bgimage_bottom(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ color = border_color }, cr, width, height, border_bottom)
end

local border_left = border.directions{ "left" }
local function draw_tb_border_bgimage_left(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ color = border_color }, cr, width, height, border_left)
end

local border_right = border.directions{ "right" }
local function draw_tb_border_bgimage_right(context, cr, width, height)
    local c = context["client"]
    local border_color = gcolor(capi.client.focus == c and beautiful.border_focus or beautiful.border_normal)
    border:draw({ color = border_color }, cr, width, height, border_right)
end

local function create_titlebars(c)
    awful.titlebar(c,
                   {
                       position = "top",
                       size = beautiful.border_width,
                       bgimage = draw_tb_border_bgimage_top,
                   }
    ) : setup({ widget = wibox.container.background })
    awful.titlebar(c,
                   {
                       position = "bottom",
                       size = beautiful.border_width,
                       bgimage = draw_tb_border_bgimage_bottom,
                   }
    ) : setup({ widget = wibox.container.background })
    awful.titlebar(c,
                   {
                       position = "left",
                       size = beautiful.border_width,
                       bgimage = draw_tb_border_bgimage_left,
                   }
    ) : setup({ widget = wibox.container.background })
    awful.titlebar(c,
                   {
                       position = "right",
                       size = beautiful.border_width,
                       bgimage = draw_tb_border_bgimage_right,
                   }
    ) : setup({ widget = wibox.container.background })
end

capi.client.connect_signal("request::titlebars", create_titlebars)

local function reset_decoration(c)
    if not c.borderless and not c.maximized then
        c.border_width = 0
    else
        c.border_width = 0
    end
    if c.maximized -- and not shared.var.enable_titlebar
    then
        shared.client.titlebar_hide(c)
    else
        if c.has_titlebar_enabled then
            shared.client.titlebar_show(c)
        end
    end
end

local function manage_cb(c)
    c.has_titlebar = false
    c.has_titlebar_enabled = shared.var.enable_titlebar
   reset_decoration(c)
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
      rule = { class = "Plank", type = "dock" },
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
}

return nil

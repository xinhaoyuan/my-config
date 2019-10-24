local capi = {
   awesome = awesome,
   screen = screen,
   mouse = mouse,
   client = client,
   root = root,
}

local shared = require((...):match("(.-)[^%.]+$") .. "shared")
shared.screen = {}

local awful  = require("awful")
local beautiful = require("beautiful")
local watch = require("awful.widget.watch")
local wibox  = require("wibox")
local gtimer  = require("gears.timer")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local gmath = require("gears.math")
local waffle = require("waffle")
local calendar = require("calendar.calendar")
local menu = require("my-menu")
local dpi = require("beautiful.xresources").apply_dpi
local yams = require("yams")
local yams_switcher = yams.create{panel = false}
local fallback = require("fallback")
local fixed_margin = require("fixed_margin")
local fixed_place = require("fixed_place")
local fixed_align = require("fixed_align")
local masked_imagebox = require("masked_imagebox")
local cbg = require("contextual_background")
local aux = require("aux")
local icons = require("icons")
require("manage_ticket")
local revelation = require("revelation")
revelation.init()
revelation.property_to_watch.maximized = false

-- helper functions

local table_join = awful.util.table.join
local delayed = gtimer.delayed_call

local function open_tmux_session(name)
   shared.action.terminal({"tmux", "new", "-As", name})
end

local function go_by_direction(dir, with_client)
   if with_client then
      local c = capi.client.focus
      awful.screen.focus_bydirection(dir, c.screen)
      c:move_to_screen(capi.mouse.screen.index)
      c:emit_signal("request::activate", "mouse.resize", {raise = true})
   else
      awful.screen.focus_bydirection(dir)
   end
end

-- add machi layout

local machi = require("layout-machi")

beautiful.layout_machi = machi.get_icon()
machi.default_editor.set_gap(beautiful.useless_gap * 2, beautiful.useless_gap * 2)

local alayout = require("awful.layout")
alayout.layouts = {
   machi.default_layout,
   alayout.suit.tile,
   alayout.suit.tile.left,
   alayout.suit.tile.bottom,
   alayout.suit.tile.top,
   alayout.suit.fair,
   alayout.suit.fair.horizontal,
   alayout.suit.spiral,
   alayout.suit.spiral.dwindle,
   alayout.suit.magnifier,
   alayout.suit.corner.nw,
   -- alayout.suit.corner.ne,
   -- alayout.suit.corner.sw,
   -- alayout.suit.corner.se,
}

-- Define the tag list upfront for keybindings

local root_buttons = awful.util.table.join(
   awful.button({ }, 3, function () waffle:set_gravity("center"); waffle:show() end),
   -- awful.button({ }, 3, function () menu:show() end),
   capi.root.buttons()
)

capi.root.buttons(root_buttons)

local fortune_widget = wibox.widget {
   align = "center",
   forced_height = beautiful.bar_height,
   widget = wibox.widget.textbox
}
fortune_widget.watch = require("watchcommand").create({"fortune", "-s"}, 300)
fortune_widget.watch:connect_signal(
   "property::output",
   function (watch)
      local raw = watch.output:gsub("\n", " "):gsub("%s+", " "):match("^%s*(.-)%s*$")
      fortune_widget:set_text(" << " .. raw .. " >> ")
      fortune_widget.fortune_raw = raw
   end
)

function shared.screen.toggle_fortune()
   fortune_widget:set_visible(not fortune_widget:get_visible())
end

function shared.screen.get_fortune()
   return fortune_widget.fortune_raw
end

-- Screen bar

local my_widgets = {}
local my_tray = wibox.widget.systray()
local my_tag_list_buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ "Mod4" }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ "Mod4" }, 3, awful.client.toggletag),
   awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

local client_menu_selected = nil
local client_menu = awful.menu({
      { "Close", function () client_menu_selected:kill() end },
      { "(Un)maximize", function () client_menu_selected.maximized = not client_menu_selected.maximized end },
})

local default_icon = gcolor.recolor_image(icons.terminal, beautiful.fg_normal)

local property_to_text = {
   {"sticky", "S"},
   {"ontop", "T"},
   {"maximized", "M"},
   {"floating", "F"},
}

local function tasklist_update_function(widget, c, index, objects)
    assert(widget:get_children_by_id("default_icon")[1].is_masked_imagebox)
    local sb = widget:get_children_by_id("status_role")[1]
    local bgb = widget:get_children_by_id("my_background_role")[1]
    local status_text = ""
    local prop = {}
    for _, pp in ipairs(property_to_text) do
        local key = pp[1]
        if c.saved and c.saved[key] ~= nil then
            prop[key] = c.saved[key]
        elseif c[key] ~= nil then
            prop[key] = c[key]
        end
    end
    for _, pp in ipairs(property_to_text) do
        local key, text = table.unpack(pp)
        if prop[key] == true then
            if key ~= "floating" or not prop.maximized then
                status_text = status_text .. text
            end
        end
    end
    if sb then
        if #status_text > 0 then
            sb.text = status_text
        else
            sb.text = ""
        end
    end
    bgb:set_context_transform_function({focus = client.focus == c, minimized = c.minimized, is_odd = index % 2 == 1})
end

local function tasklist_create_function(widget, c, index, objects)
    local ib = widget:get_children_by_id("default_icon")[1]
    masked_imagebox(ib)

    local al = widget:get_children_by_id("action_layer")[1]
    local ac = widget:get_children_by_id("action_container")[1]

    for _, b in ipairs(widget:get_children_by_id("base_action")) do
        b:buttons(awful.util.table.join(
                       awful.button({ }, 1, function ()
                               if c == capi.client.focus then
                                   capi.mouse.coords({
                                           x = c.x + c.width / 2,
                                           y = c.y + c.height / 2,
                                                     }, true)
                                   delayed(function () awful.mouse.client.move(c) end)
                               else
                                   -- Without this, the following
                                   -- :isvisible() makes no sense
                                   c.minimized = false
                                   if not c:isvisible() then
                                       awful.tag.viewonly(c:tags()[1])
                                   end
                                   -- This will also un-minimize
                                   -- the client, if needed
                                   capi.client.focus = c
                                   c:raise()
                               end
                       end),
                       awful.button({ }, 2,
                           function ()
                               shared.client.titlebar_enable(c)
                           end
                       ),
                       awful.button({ }, 3,
                           function ()
                               if c == capi.client.focus then
                                   delayed(function () awful.mouse.client.resize(c, "bottom_right") end)
                               elseif c.minimized then
                                   c:kill()
                               end
                           end
                       ),
                       awful.button({ }, 4,
                           function ()
                               if not c.maximized then
                                   shared.client.enlarge(c)
                               end
                           end
                       ),
                       awful.button({ }, 5,
                           function ()
                               shared.client.shrink(c)
                           end
                       )
        ))
    end

    if al then
        widget:connect_signal(
            "mouse::enter",
            function (w)
                al.visible = true
            end
        )
        widget:connect_signal(
            "mouse::leave",
            function (w)
                al.visible = false
            end
        )
    end
    if ac then
        ac:set_children({
                awful.titlebar.widget.floatingbutton (c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton   (c),
                awful.titlebar.widget.ontopbutton    (c),
                layout = wibox.layout.fixed.horizontal()
        })
    end
    tasklist_update_function(widget, c, index, objects)
end

local alt_color_cache = {}
local function alt_color(color)
   if alt_color_cache[color] == nil then
      local comp = aux.color.from_string(color)
      for i = 1, 3 do
         if comp[i] > 0.5 then
            comp[i] = comp[i] - 0.05
         else
            comp[i] = comp[i] + 0.05
         end
      end
      alt_color_cache[color] = comp:to_string()
   end
   return alt_color_cache[color]
end

local tasklist_template = {
    {
        {
            {
                {
                    {
                        {
                            widget = awful.widget.clienticon,
                        },
                        {
                            id = "default_icon",
                            image = default_icon,
                            widget = wibox.widget.imagebox,
                        },
                        widget = fallback,
                    },
                    right = dpi(3),
                    widget = wibox.container.margin,
                },
                {
                    id = "text_role",
                    widget = wibox.widget.textbox,
                },
                {
                    {
                        {
                            id = "status_role",
                            widget = wibox.widget.textbox,
                        },
                        fg_function = function (context)
                            if context.focus or context.minimized then
                                return beautiful.special_focus
                            else
                                return beautiful.special_normal
                            end
                        end,
                        widget = cbg
                    },
                    left = dpi(3),
                    widget = wibox.container.margin,
                },
                layout = wibox.layout.align.horizontal,
            },
            left  = dpi(5),
            right = dpi(5),
            widget = wibox.container.margin
        },
        {
            {
                {
                    nil,
                    {
                        id = "base_action",
                        content_fill_horizontal = true,
                        widget = fixed_place,
                    },
                    {
                        {
                            {
                                id = "action_container",
                                layout = wibox.layout.fixed.horizontal,
                            },
                            left = dpi(10),
                            widget = wibox.container.margin,
                        },
                        shape = gshape.rectangular_tag,
                        bg_function = function (context)
                            local ret
                            ret = beautiful.bg_normal
                            if context.is_odd then
                                ret = alt_color(ret)
                            end
                            return ret
                        end,
                        widget = cbg,
                    },
                    expand = "inside",
                    widget = wibox.layout.align.horizontal,
                },
                id = "action_layer",
                visible = false,
                bg_function = function (context)
                    local ret
                    if context.focus then
                        ret = beautiful.bg_focus
                    elseif context.minimized then
                        ret = beautiful.bg_minimize
                    else
                        ret = beautiful.bg_normal
                    end
                    if context.is_odd then
                        ret = alt_color(ret)
                    end
                    return ret
                end,
                widget = cbg,
            },
            content_fill_horizontal = true,
            --- fill_horizontal = true,
            widget = fixed_place,
        },
        layout = wibox.layout.stack,
    },
    id     = "my_background_role",
    fg_function = function (context)
        if context.focus or context.minimized then
            return beautiful.fg_focus
        else
            return beautiful.fg_normal
        end
    end,
    bg_function = function (context)
        local ret
        if context.focus then
            ret = beautiful.bg_focus
        elseif context.minimized then
            ret = beautiful.bg_minimize
        else
            ret = beautiful.bg_normal
        end
        if context.is_odd then
            ret = alt_color(ret)
        end
        return ret
    end,
    widget = cbg,
    create_callback = tasklist_create_function,
    update_callback = tasklist_update_function,
}


-- local my_tasklist_buttons = awful.util.table.join(
--     awful.button({ }, 1, function (c)
--             if c == capi.client.focus then
--                 capi.mouse.coords({
--                         x = c.x + c.width / 2,
--                         y = c.y + c.height / 2,
--                                   }, true)
--                 awful.mouse.client.move(c)
--             else
--                 -- Without this, the following
--                 -- :isvisible() makes no sense
--                 c.minimized = false
--                 if not c:isvisible() then
--                     awful.tag.viewonly(c:tags()[1])
--                 end
--                 -- This will also un-minimize
--                 -- the client, if needed
--                 capi.client.focus = c
--                 c:raise()
--             end
--     end),
--     awful.button({ }, 2,
--         function (c)
--             shared.client.titlebar_enable(c)
--         end
--     ),
--     awful.button({ }, 3,
--         function (c)
--             if c == capi.client.focus then
--                 awful.mouse.client.resize(c, "bottom_right")
--             elseif c.minimized then
--                 c:kill()
--             end
--         end
--     ),
--     awful.button({ }, 4,
--         function (c)
--             if not c.maximized then
--                 shared.client.enlarge(c)
--             end
--         end
--     ),
--     awful.button({ }, 5,
--         function (c)
--             shared.client.shrink(c)
--         end
--     )
-- )

local current_screen = nil
local primary_screen = nil

-- a basic stable sort
local function sort(l, c)
   local ret = {}
   for i = 1, #l, 1 do
      ret[i] = l[i]
      for j = i, 2, -1 do
         local to_swap
         if c == nil then
            to_swap = ret[j - 1] > ret[j]
         else
            to_swap = c(ret[j], ret[j - 1])
         end
         if to_swap then
            local tmp = ret[j - 1]
            ret[j - 1] = ret[j]
            ret[j] = tmp
         else
            break
         end
      end
   end
   return ret
end

local alt_color_cache = {}
local function alt_color(color)
   if alt_color_cache[color] == nil then
      local comp = aux.color.from_string(color)
      for i = 1, 3 do
         if comp[i] > 0.5 then
            comp[i] = comp[i] - 0.10
         else
            comp[i] = comp[i] + 0.10
         end
      end
      alt_color_cache[color] = comp:to_string()
   end
   return alt_color_cache[color]
end

local function setup_screen(scr)
   local s = scr.index

   scr.mypromptbox = awful.widget.prompt()

   my_widgets[s] = {}
   local tasklist = awful.widget.tasklist {
      screen = s,
      filter = function (c, s)
         if not awful.widget.tasklist.filter.currenttags(c, s) then
            return false
         end
         return not (c:isvisible() and shared.var.hide_clients_with_titlebars and c.has_titlebar)
      end,
      -- buttons = my_tasklist_buttons,
      style = { font = beautiful.font },
      layout = beautiful.tasklist_layout[beautiful.bar_style],
      source = function ()
         -- Sort clients with their constant ids to make the order stable.
         local cls = awful.widget.tasklist.source.all_clients()
         table.sort(cls,
                    function (a, b)
                       -- this makes minimized windows appear at last
                       -- if a.minimized ~= b.minimized then return b.minimized else return a.window < b.window end
                       return a.manage_ticket < b.manage_ticket
                    end
         )
         return cls
      end,
      -- update_function = function (w, b, l, d, objects, args)
      --    -- not used any more. just for future reference

      --    -- -- Reorder the clients so that floating clients are on the right side
      --    -- fl_clients = {}
      --    -- clients = {}
      --    -- for i, obj in ipairs(objects) do
      --    --    if obj.floating or obj.maximized or obj.maximized_horizontal or obj.maximized_vertical then
      --    --       fl_clients[#fl_clients + 1] = obj
      --    --    else
      --    --       clients[#clients + 1] = obj
      --    --    end
      --    -- end
      --    -- for i, obj in ipairs(fl_clients) do
      --    --    clients[#clients + 1] = obj
      --    -- end

      --    -- A hacky way to alternate the colors of tasklist items
      --    awful.widget.common.list_update(
      --       w, b,
      --       function (c, tb)
      --          local ret = table.pack(l(c, tb))
      --          -- bg is stored as [2]
      --          -- fg is embedded as color='...' in [1]
      --          if c.minimized and c.saved and not c.saved.minimized then
      --              local fg = beautiful.tasklist_fg_normal or beautiful.fg_normal
      --              ret[1] = ret[1]:gsub("'#(%w+)'", "'" .. fg .. "'")
      --              ret[2] = beautiful.tasklist_bg_normal or beautiful.bg_normal
      --          end
      --          if tb.is_odd_child then
      --             ret[2] = alt_color(ret[2])
      --          end
      --          return table.unpack(ret)
      --       end,
      --       d, objects, args)
      -- end,
      widget_template = tasklist_template,
   }
   tasklist = {
      tasklist,
      fortune_widget,
      widget = fallback,
   }
   my_widgets[s].tasklist = tasklist

   my_widgets[s].tag_list = awful.widget.taglist(
      s, function (t) return true end, my_tag_list_buttons,
      {
         font = beautiful.font
      }
   )

   my_widgets[s].wibar = awful.wibar({
         screen = s,
         fg = beautiful.fg_normal,
         bg = beautiful.bar_style == "split" and "#00000000" or beautiful.bg_normal,
         height = beautiful.bar_height + beautiful.border_width,
         position = "bottom",
         border_width = 0,
         cursor = "cross",
   })

   local left_layout = wibox.layout.fixed.horizontal()
   local layoutbox = awful.widget.layoutbox(s)
   masked_imagebox(layoutbox.imagebox)
   my_widgets[s].indicator = wibox.widget {
       layoutbox,
       fg_function = {"fg_"},
       bg_function = {"bg_"},
       widget = cbg
   }
   my_widgets[s].indicator:buttons(
      awful.util.table.join(
          awful.button({ }, 1, function () revelation{curr_tag_only = true} end),
          awful.button({ }, 3, function () waffle:set_gravity("southwest"); waffle:show() end),
          awful.button({ }, 4, function () awful.layout.inc( 1) end),
          awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   left_layout:add(my_widgets[s].indicator)
   left_layout:add(my_widgets[s].tag_list)
   left_layout:add(scr.mypromptbox)
   local right_layout = wibox.widget {
      spacing        = dpi(5),
      spacing_widget = { color = beautiful.bg_normal, widget = wibox.widget.separator },
      layout         = wibox.layout.fixed.horizontal
   }

   if scr == primary_screen then
      right_layout:add(my_tray)
   end

   local clock = wibox.widget.textclock("<span color='" .. beautiful.border_focus .. "'>%m<b>%d</b></span>%H<b>%M</b> ")
   clock:set_font(beautiful.font)
   local calendar_widget = calendar({
         fdow = 7,
         html = "<span font_desc='" .. beautiful.font_mono .. "'>\n%s</span>",
         today_color = beautiful.special_normal,
         position = "bottom_right",
   })
   calendar_widget:attach(clock)
   right_layout:add(clock)

   local layout

   if beautiful.bar_style == "split" then
      local middle = fixed_margin(
         wibox.widget {
            {
               tasklist,
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
            },
            draw_empty = false,
            top = beautiful.border_width,
            color = beautiful.border_focus,
            widget = wibox.container.margin,
      })
      middle = {
         {
            {
               wibox.widget {
                  buttons = root_buttons,
                  content_fill_horizontal = true,
                  widget = wibox.container.place,
               },
               draw_empty = false,
               left = beautiful.border_width,
               right = beautiful.border_width,
               color = beautiful.border_focus,
               widget = wibox.container.margin,
            },
            middle,
            {
               {
                  buttons = root_buttons,
                  content_fill_horizontal = true,
                  widget = wibox.container.place,
               },
               draw_empty = false,
               left = beautiful.border_width,
               right = beautiful.border_width,
               color = beautiful.border_focus,
               widget = wibox.container.margin,
            },
            expand = "outside",
            layout = fixed_align.horizontal,
         },
         content_fill_horizontal = true,
         widget = wibox.container.place,
      }
      layout = wibox.widget {
         {
            {
               left_layout,
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
            },
            draw_empty = true,
            top = beautiful.border_width,
            color = beautiful.border_focus,
            widget = wibox.container.margin,
         },
         {
            middle,
            {
               {
                  buttons = root_buttons,
                  fill_horizontal = true,
                  widget = wibox.container.place,
               },
               draw_empty = true,
               left = beautiful.border_width,
               right = beautiful.border_width,
               color = beautiful.border_focus,
               widget = wibox.container.margin,
            },
            draw_last = true,
            widget = fallback,
         },
         {
            {
               {
                  right_layout,
                  draw_empty = false,
                  left = dpi(5),
                  widget = wibox.container.margin,
               },
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
            },
            draw_empty = true,
            top = beautiful.border_width,
            color = beautiful.border_focus,
            widget = wibox.container.margin,
         },
         layout = wibox.layout.align.horizontal,
      }
   else
      layout = wibox.widget {
         {
            left_layout,
            {
               tasklist,
               content_fill_horizontal = true,
               widget = wibox.container.place,
            },
            {
               right_layout,
               left = dpi(5),
               widget = wibox.container.margin,
            },
            layout = wibox.layout.align.horizontal,
         },
         top = beautiful.border_width,
         color = beautiful.border_focus,
         widget = wibox.container.margin,
      }
   end
   my_widgets[s].wibar:set_widget(layout)
end

-- Avoid nested call of reset_widgets
local reset_widgets_flag = false

local function reset_widgets()
    for _, w in ipairs(my_widgets) do
        w.wibar:remove()
    end
    my_widgets = {}
    current_screen = nil
    primary_screen = capi.screen.primary

    for scr in capi.screen do
        setup_screen(scr)
    end

    shared.action.wallpaper_setup(true)
    reset_widgets_flag = false
end

function schedule_reset_widgets()
    if reset_widgets_flag then return end
    reset_widgets_flag = true
    delayed(reset_widgets)
end

table.insert(shared.on_start_functions, schedule_reset_widgets)

capi.screen.connect_signal("list", schedule_reset_widgets)
capi.screen.connect_signal("primary_changed", schedule_reset_widgets)

capi.root.keys(
   awful.util.table.join(
      capi.root.keys(),
      awful.key({ "Mod4" }, "F12", function () waffle:set_gravity("center"); waffle:show() end),
      awful.key({ "Mod4" }, ";",
         function ()
            awful.prompt.run {
               prompt       = "Run Lua code: ",
               font         = beautiful.font,
               textbox      = awful.screen.focused().mypromptbox.widget,
               exe_callback = awful.util.eval,
               history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
         end,
         {description = "lua execute prompt", group = "awesome"})
))

gtimer {
    timeout = 0.5,
    autostart = true,
    callback = function()
        local nscreen = capi.mouse.screen.index
        if nscreen ~= current_screen then
            if current_screen ~= nil then
                my_widgets[current_screen].indicator:set_context_transform_function(nil)
            end
            my_widgets[nscreen].indicator:set_context_transform_function({focus = true})
            -- switch active screen
            current_screen = nscreen
        end
    end
}

-- base keys and buttons
local global_keys = table_join(
   awful.key({ "Mod1" }, "Tab",
      function ()
         yams_switcher.start(nil)
   end),
   awful.key({ "Mod4" }, "/",               function () machi.default_editor.start_interactive() end),
   awful.key({ "Mod4" }, "[",               function () alayout.inc(alayout.layouts, -1) end),
   awful.key({ "Mod4" }, "]",               function () alayout.inc(alayout.layouts, 1) end),
   awful.key({ "Mod4" }, "Up",              function () go_by_direction("up") end),
   awful.key({ "Mod4" }, "Left",            function () go_by_direction("left") end),
   awful.key({ "Mod4" }, "Down",            function () go_by_direction("down") end),
   awful.key({ "Mod4" }, "Right",           function () go_by_direction("right") end),
   awful.key({ "Control", "Mod4" }, "Up",   function () go_by_direction("up", true) end),
   awful.key({ "Control", "Mod4" }, "Left", function () go_by_direction("left", true) end),
   awful.key({ "Control", "Mod4" }, "Down", function () go_by_direction("down", true) end),
   awful.key({ "Control", "Mod4" }, "Right",function () go_by_direction("right", true) end),
   awful.key({ }, "XF86AudioLowerVolume",   function () shared.action.audio_setup("volume-adjust", -5) end),
   awful.key({ }, "XF86AudioRaiseVolume",   function () shared.action.audio_setup("volume-adjust",  5) end),
   awful.key({ }, "XF86AudioMute",          function () shared.action.audio_setup("mute-toggle") end),
   awful.key({ }, "XF86MonBrightnessUp",    function () awful.spawn("xbacklight -inc 5", false) end),
   awful.key({ }, "XF86MonBrightnessDown",  function () awful.spawn("xbacklight -dec 5", false) end),
   awful.key({ "Mod4" }, "Escape",          function () revelation{curr_tag_only = true} end),
   awful.key({ "Mod4" }, "Return",          function () shared.action.terminal() end),
   awful.key({ "Mod4" }, "w",               function () shared.action.web_browser() end),
   awful.key({ "Mod4" }, "e",               function () shared.action.file_manager() end),
   awful.key({ "Mod4" }, "l",               function () shared.action.screen_locker() end),
   awful.key({ "Mod4" }, "\\",              function () shared.action.launcher() end),
   awful.key({ "Mod4", "Shift" }, "F12",    function () shared.action.app_finder() end),
   awful.key({ "Mod4" }, "F1",              function () open_tmux_session("F1") end),
   awful.key({ "Mod4" }, "F2",              function () open_tmux_session("F2") end),
   awful.key({ "Mod4" }, "F3",              function () open_tmux_session("F3") end),
   awful.key({ "Mod4" }, "F4",              function () open_tmux_session("F4") end),
   -- keep the both ways of showing the desktop, not sure which one is better for now.
   awful.key({ "Mod4" }, "d",               function ()
         local clients = {}
         local has_visible = false
         for _, c in ipairs(capi.client.get()) do
            if c:isvisible() and awful.client.focus.filter(c) then
               c.orig_minimized = c.minimized
               c.minimized = true
               has_visible = true
            end
         end

         if not has_visible then
            clients = {}
            for _, c in ipairs(capi.client.get()) do
               if c.orig_minimized ~= nil then
                  clients[#clients + 1] = c
               end
            end

            -- I thought I should put newer client later. Turned out to be the reversed way.
            table.sort(
               clients,
               function (a, b)
                  return a.focus_timestamp > b.focus_timestamp
               end
            )

            for _, c in ipairs(clients) do
               c.minimized = c.orig_minimized
               c.orig_minimized = nil
            end
         end
   end),
   awful.key({ "Mod4" }, "q",               function ()
         local to_restore = true
         for s in capi.screen do
            if #s.selected_tags > 0 then
               to_restore = false
               s.orig_selected_tags = s.selected_tags
               awful.tag.viewnone(s)
            end
         end

         if not to_restore then return end
         for s in capi.screen do
            if s.orig_selected_tags ~= nil then
               awful.tag.viewmore(s.orig_selected_tags, s)
               s.orig_selected_tags = nil
            end
         end
   end),
   awful.key({ "Mod4", "Control" }, "r",      capi.awesome.restart),
   awful.key({ "Mod4", "Control" }, "Escape", capi.awesome.quit)
)

-- tags and layouts

shared.screen.tags = { "1", "2", "3", "4" }

for i = 1, #shared.screen.tags do
   local key = tostring(i)
   global_keys =
      table_join(
         awful.key({ "Mod4" }, key, function () awful.screen.focused().tags[i]:view_only() end),
         awful.key({ "Mod4", "Control" }, key, function () awful.tag.viewtoggle(awful.screen.focused().tags[i]) end),
         awful.key({ "Mod4", "Shift" }, key, function ()
               local c = capi.client.focus
               if c == nil then return end
               c:toggle_tag(c.screen.tags[i])
         end),
         global_keys)
end

capi.root.keys(table_join(capi.root.keys(), global_keys))

-- initialize tags for each screen

awful.screen.connect_for_each_screen(
   function (s)
      for i, t in ipairs(shared.screen.tags) do
         local tag = awful.tag.add(t, { screen = s, layout = alayout.layouts[1], layouts = alayout.layouts })
      end

      s.tags[1]:view_only()

      -- fix window geometry
      s:connect_signal(
         "property::geometry",
         function (s)
            local clients = {}
            for _, c in ipairs(s.all_clients) do
               if not c.minimized and c.maximized then
                  c.maximized = false
                  table.insert(clients, c)
               end
            end

            delayed(function ()
                  for _, c in ipairs(clients) do
                     c.maximized = true
                  end
            end)
      end)
   end
)

return nil

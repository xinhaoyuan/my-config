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
local mycalendar = require("my-calendar")
local dpi = require("beautiful.xresources").apply_dpi
local yams = require("yams")
local yams_switcher = yams.create{opacity_other = 1, panel = false}
local fallback = require("fallback")
local fixed_margin = require("fixed_margin")
local fixed_place = require("fixed_place")
local fixed_align = require("fixed_align")
local masked_imagebox = require("masked_imagebox")
local border = require("border-theme")
local debug_container = require("debug_container")
local tasklist = require("config.tasklist")
local cbg = require("contextual_background")
local fts = require("hotpot").focus_timestamp
local aux = require("aux")
local icons = require("icons")
local orgenda = require("orgenda")
require("manage_ticket")

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

local size_index = shared.size_index
local dual_size_index = shared.dual_size_index
local top_index = shared.top_index
local bottom_index = shared.bottom_index
local left_index = shared.left_index
local right_index = shared.right_index
local direction_index = shared.direction_index
local dual_direction_index = shared.dual_direction_index
local gravity_index = shared.gravity_index

-- add machi layout

local machi = require("layout-machi")

beautiful.layout_machi = machi.get_icon()
machi.default_editor.set_gap(beautiful.useless_gap, beautiful.useless_gap)

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
    awful.button({ }, 3, function () waffle:show(nil, { anchor = "mouse" }) end),
    capi.root.buttons()
)

capi.root.buttons(root_buttons)

local fortune_widget = wibox.widget {
    valign = "center",
    align = "center",
    forced_height = beautiful.bar_height,
    buttons = awful.util.table.join(awful.button({ }, 3, function () waffle:show(nil, { anchor = "mouse" }) end)),
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
    -- TODO: Investigate why using set_visible would not let it show up.
    if fortune_widget:get_forced_width() == 0 then
        fortune_widget:set_forced_width(nil)
    else
        fortune_widget:set_forced_width(0)
    end
end

function shared.screen.get_fortune()
   return fortune_widget.fortune_raw
end

-- Screen bar

local function switch_to_or_go_last(tag)
    local screen = tag.screen
    if #screen.selected_tags == 1 and screen.selected_tags[1] == tag then
        awful.tag.history.restore(screen)
    else
        tag:view_only()
    end
end

local my_wibars = {}
local my_tray
my_tray = wibox.widget.systray()
my_tray.horizontal = direction_index[shared.var.bar_position] == "horizontal"
my_tray.base_size = beautiful.bar_height
local bar_tray_wrapper = wibox.widget {
    my_tray,
    widget = wibox.container.background
}

function shared.screen.detach_tray_widget()
    bar_tray_wrapper.widget = nil
    return my_tray
end

function shared.screen.attach_tray_widget()
    bar_tray_wrapper.widget = my_tray
end

local my_tag_list_buttons = awful.util.table.join(
   awful.button({ }, 1, switch_to_or_go_last),
   awful.button({ "Mod4" }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ "Mod4" }, 3, awful.client.toggletag),
   awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

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

local function rounded_rect_with_corners(cr, width, height, radius, corners)
    radius = math.min(radius, width / 2, height / 2)
    cr:move_to(radius, 0)
    if corners.top_right then
        cr:arc( width-radius, radius       , radius, 3*(math.pi/2),    math.pi*2  )
    else
        cr:line_to(width, 0)
        cr:line_to(width, radius)
    end
    if corners.bottom_right then
        cr:arc( width-radius, height-radius, radius,    math.pi*2 ,    math.pi/2  )
    else
        cr:line_to(width, height)
        cr:line_to(width - radius, height)
    end
    if corners.bottom_left then
        cr:arc( radius      , height-radius, radius,    math.pi/2 ,    math.pi    )
    else
        cr:line_to(0, height)
        cr:line_to(0, height - radius)
    end
    if corners.top_left then
        cr:arc( radius      , radius       , radius,    math.pi   , 3*(math.pi/2) )
    else
        cr:line_to(0, 0)
        cr:line_to(radius, 0)
    end
    cr:close_path()
end

local function with_border(args)
    args = args or {}
    local widget
    local inner_widget = wibox.widget {
        {
            args.widget,
            widget = fixed_margin,
        },
        shape = beautiful.border_radius ~= nil and
            function (cr, width, height)
                rounded_rect_with_corners(cr, width, height,
                                          beautiful.border_radius -
                                          beautiful.border_width,
                                          {
                                              top_left = widget.widget.top > 0 and widget.widget.left > 0,
                                              top_right = widget.widget.top > 0 and widget.widget.right > 0,
                                              bottom_left = widget.widget.bottom > 0 and widget.widget.left > 0,
                                              bottom_right = widget.widget.bottom > 0 and widget.widget.right > 0,
                })
            end,
        bg = beautiful.bg_normal,
        widget = wibox.container.background
    }
    widget = wibox.widget {
        {
            inner_widget,
            top = args.top and beautiful.border_width,
            left = args.left and beautiful.border_width,
            right = args.right and beautiful.border_width,
            bottom = args.bottom and beautiful.border_width,
            draw_empty = args.draw_empty,
            widget = fixed_margin,
        },
        bgimage = function (context, cr, width, height)
            if width == 0 or height == 0 then return end
            border:draw({ theme = border_theme,
                          color = beautiful.border_focus }, cr, width, height,
                border.directions{
                    widget.widget.top > 0 and "top",
                    widget.widget.left > 0 and "left",
                    widget.widget.right > 0 and "right",
                    widget.widget.bottom > 0 and "bottom"
            })
        end,
        shape = beautiful.border_radius ~= nil and
            function (cr, width, height)
                rounded_rect_with_corners(cr, width, height,
                                          beautiful.border_radius, {
                                              top_left = widget.widget.top > 0 and widget.widget.left > 0,
                                              top_right = widget.widget.top > 0 and widget.widget.right > 0,
                                              bottom_left = widget.widget.bottom > 0 and widget.widget.left > 0,
                                              bottom_right = widget.widget.bottom > 0 and widget.widget.right > 0,
                })
            end,
        widget = wibox.container.background
    }
    return widget
end

local space_filler = wibox.widget {
    forced_width = beautiful.useless_gap,
    widget = wibox.container.constraint
}

local function with_top_border(widget)
    return with_border { widget = widget, top = true }
end

local space_filler_left_with_top_border = with_top_border {
    {
        {
            beautiful.sep_widget,
            forced_width = dpi(6),
            content_fill_vertical = true,
            content_fill_horizontal = true,
            widget = wibox.container.place
        },
        halign = "left",
        widget = fixed_place,
    },
    width = beautiful.border_width * 2 + beautiful.useless_gap,
    strategy = "min",
    widget = wibox.container.constraint
}

local space_filler_right_with_top_border = with_top_border {
    {
        {
            beautiful.sep_widget,
            forced_width = dpi(6),
            content_fill_vertical = true,
            content_fill_horizontal = true,
            widget = wibox.container.place
        },
        halign = "right",
        widget = fixed_place,
    },
    width = beautiful.border_width * 2 + beautiful.useless_gap,
    strategy = "min",
    widget = wibox.container.constraint
}

local function set_expanded(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler_left_with_top_border})
    space_filler_right:set_children({space_filler_right_with_top_border})

    bar.left_margin_container.widget.right = 0
    bar.middle_margin_container.widget.left = 0
    bar.middle_margin_container.widget.right = 0
    bar.right_margin_container.widget.left = 0
    bar.right_margin_container.widget.widget.widget.left = 0
end

local function set_splitted_no_min(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler_no_min})
    space_filler_right:set_children({space_filler_no_min})

    bar.left_margin_container.widget.right = beautiful.border_width
    bar.middle_margin_container.widget.left = beautiful.border_width
    bar.middle_margin_container.widget.right = beautiful.border_width
    bar.right_margin_container.widget.left = beautiful.border_width
    bar.right_margin_container.widget.widget.widget.left =
        bar.screen == capi.screen.primary and
        beautiful.border_radius and
        beautiful.border_radius - beautiful.border_width or 0
end

local function set_splitted(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler})
    space_filler_right:set_children({space_filler})

    bar.left_margin_container.widget.right = beautiful.border_width
    bar.middle_margin_container.widget.left = beautiful.border_width
    bar.middle_margin_container.widget.right = beautiful.border_width
    bar.right_margin_container.widget.left = beautiful.border_width
    bar.right_margin_container.widget.widget.widget.left =
        bar.screen == capi.screen.primary and
        beautiful.border_radius and
        beautiful.border_radius - beautiful.border_width or 0
end

capi.awesome.connect_signal(
    "tasklist::update::before",
    function (s)
        if beautiful.bar_style == "split" then
            set_splitted(s.widgets.wibar)
            return
        elseif beautiful.bar_style == "simple" then
            set_expanded(s.widgets.wibar)
            return
        elseif beautiful.bar_style ~= "auto" then
            print("Expecting auto bar_style but got", beautiful.bar_style)
        end

        local has_maximized = false
        for _, c in ipairs(s.clients) do
            has_maximized = has_maximized or c.maximized
        end
        if has_maximized then
            set_expanded(s.widgets.wibar)
        else
            set_splitted(s.widgets.wibar)
        end
    end
)

-- Calendar popup

local today = os.date("*t")
local active_dates = {}
local cal_widget = wibox.widget {
    date = os.date('*t'),
    font = beautiful.font,
    week_numbers = true,
    -- start_sunday = true,
    long_weekdays = true,
    spacing = 0,
    fn_embed = function (widget, flag, date)
        local inverted = false
        if flag == 'header' then
            widget.font = beautiful.fontname_normal..' 12'
        end

        if flag == "normal" or flag == "focus" then
            if today.year == date.year and today.month == date.month and today.day == date.day then
                inverted = true
            end

            if active_dates[date.year] and active_dates[date.year][date.month] and active_dates[date.year][date.month][date.day] then
                widget = wibox.widget {
                    {
                        {
                            {
                                text = '•',
                                widget = wibox.widget.textbox,
                            },
                            fg_function = function(context)
                                if context.inverted then return beautiful.special_focus else return beautiful.special_normal end
                            end,
                            widget = cbg,
                        },
                        halign = 'left',
                        widget = wibox.container.place
                    },
                    widget,
                    layout = wibox.layout.stack,
                }
            end
        end

        if inverted then
            return wibox.widget {
                {
                    widget,
                    margins = dpi(2),
                    widget = wibox.container.margin
                },
                fg = beautiful.fg_focus,
                bg = beautiful.bg_focus,
                context_transform_function = function (context)
                    context.inverted = true
                end,
                widget = cbg,
            }
        else
            return wibox.widget {
                widget,
                margins = dpi(2),
                widget = wibox.container.margin
            }
        end
    end,
    widget = mycalendar.month
}
gtimer {
    timeout = 10,
    autostart = true,
    callback = function()
        local new_today = os.date("*t")
        if today.day ~= new_today.day then
            today = new_today
            cal_widget:emit_signal("widget::layout_changed")
        end
    end
}
orgenda.topic:connect_signal(
    "update",
    function (_, path, items)
        active_dates = {}
        for _, item in ipairs(items) do
            if item.date then
                local parsed_array = {string.gmatch(item.date, "(%d+)-(%d+)-(%d+)")()}
                if #parsed_array == 3 then
                    parsed_array[1] = tonumber(parsed_array[1])
                    parsed_array[2] = tonumber(parsed_array[2])
                    parsed_array[3] = tonumber(parsed_array[3])
                    local y = active_dates[parsed_array[1]] or {}
                    local m = y[parsed_array[2]] or {}
                    m[parsed_array[3]] = true
                    y[parsed_array[2]] = m
                    active_dates[parsed_array[1]] = y
                end
            end
        end
        cal_widget:emit_signal("widget::layout_changed")
    end
)


local cal_popup = awful.popup {
    widget = wibox.widget {
        with_border {
            widget = {
                cal_widget,
                {
                    {
                        orgenda.widget.create {
                            width = dpi(240),
                            indent_width = dpi(26),
                            item_margin = dpi(5),
                        },
                        draw_empty = false,
                        top = dpi(12),
                        widget = fixed_margin,
                    },
                    bgimage = function(context, cr, width, height)
                        height = dpi(12)
                        beautiful.draw_separator(cr, width, height)
                    end,
                    widget = wibox.container.background
                },
                layout = wibox.layout.fixed.vertical,
            },
            top = true,
            bottom = true,
            left = true,
            right = true,
        },
        margins = beautiful.useless_gap,
        widget = wibox.container.margin
    },
    -- TODO: find out how to do this properly
    placement = function (d)
            local _, corner = awful.placement.closest_corner(mouse, {pretend=true})
            awful.placement[corner](d, {bounding_rect=mouse.screen.workarea})
    end,
    bg = "#00000000",
    ontop = true,
    visible = false,
}

local function cal_reset()
    cal_widget:set_date(nil)
    cal_widget:set_date(os.date('*t'))
end

local function cal_show()
    local _, corner = awful.placement.closest_corner(mouse, {pretend=true})
    awful.placement[corner](cal_popup, {bounding_rect=mouse.screen.workarea})
    cal_reset()
    cal_popup.visible = true
end

local function cal_hide()
    cal_popup.visible = false
end

local function cal_switch(delta)
    local date = cal_widget:get_date()
    if delta.day ~= nil then date.day = date.day + delta.day end
    if delta.month ~= nil then date.month = date.month + delta.month end
    if delta.year ~= nil then date.year = date.year + delta.year end
    cal_widget:set_date(nil)
    cal_widget:set_date(date)
end

-- Orgenda

local orgenda_counter_text_widget = wibox.widget.textbox()
local orgenda_counter_widget = wibox.widget {
    {
        {
            image = gcolor.recolor_image(icons.note, beautiful.fg_normal),
            forced_height = beautiful.bar_height - dpi(6),
            forced_width = beautiful.bar_height - dpi(6),
            widget = wibox.widget.imagebox,
        },
        valign = "center",
        widget = wibox.container.place
    },
    orgenda_counter_text_widget,
    layout = wibox.layout.fixed.horizontal
}

orgenda.topic:connect_signal(
    "update",
    function (_, path, items)
        if #items > 0 then
            orgenda_counter_widget.visible = true
            orgenda_counter_text_widget.text = tostring(#items)
        else
            orgenda_counter_widget.visible = false
        end
    end
)

local function setup_screen(scr)
   scr.mypromptbox = awful.widget.prompt()

   scr.widgets = {}
   local tasklist = tasklist.create(scr)
   local tasklist_with_fallback = {
       tasklist,
       {
           fortune_widget,
           direction = direction_index[shared.var.bar_position] == "horizontal" and "north" or "west",
           widget = wibox.container.rotate
       },
       widget = fallback,
   }

   scr.widgets.tag_list = awful.widget.taglist {
       screen = scr,
       filter = function (t) return true end,
       buttons = my_tag_list_buttons,
       layout = wibox.layout.fixed[direction_index[shared.var.bar_position]],
       widget_template = {
           {
               {
                   id = "text_role",
                   widget = wibox.widget.textbox,
               },
               halign = "center",
               valign = "center",
               forced_width = beautiful.bar_height,
               widget = wibox.container.place
           },
           id = "background_role",
           widget = wibox.container.background,
       }
   }

   scr.widgets.wibar = awful.wibar({
         screen = scr,
         fg = beautiful.fg_normal,
         ontop = false,
         bg = "#00000000",
         [size_index[shared.var.bar_position]] =
             beautiful.bar_height +
             beautiful.border_width,
         position = shared.var.bar_position,
         border_width = 0,
         cursor = "cross",
   })
   my_wibars[#my_wibars + 1] = scr.widgets.wibar

   local left_layout = wibox.layout.fixed[direction_index[shared.var.bar_position]]()
   local layoutbox = awful.widget.layoutbox{screen = scr}
   masked_imagebox.convert(layoutbox.imagebox)
   scr.widgets.indicator = wibox.widget {
       layoutbox,
       fg_function = {"fg_"},
       bg_function = {"bg_"},
       widget = cbg
   }
   scr.widgets.indicator:buttons(
      awful.util.table.join(
          awful.button({ }, 1, function () waffle:show(nil, { anchor = "mouse" }) end),
          awful.button({ }, 3, function () if client.focus ~= nil then shared.waffle.show_client_waffle(client.focus, { anchor = "mouse" }) end end),
          awful.button({ }, 4, function () awful.layout.inc( 1) end),
          awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   left_layout:add(scr.widgets.indicator)
   left_layout:add(scr.widgets.tag_list)
   left_layout:add(scr.mypromptbox)
   local right_layout = wibox.widget {
      spacing        = dpi(6),
      spacing_widget = beautiful.sep_widget,
      layout         = wibox.layout.fixed[direction_index[shared.var.bar_position]]
   }

   if scr == primary_screen then
       right_layout:add(bar_tray_wrapper)
   end

   local clock
   if direction_index[shared.var.bar_position] == "horizontal" then
       clock = wibox.widget.textclock("<span color='" .. beautiful.border_focus .. "'>%m<b>%d</b></span> %H<b>%M</b> ")
   else
       clock = wibox.widget.textclock("<span color='" .. beautiful.border_focus .. "'>%m\n<b>%d</b></span>\n%H\n<b>%M</b>")
   end
   clock.align = "center"
   clock:set_font(beautiful.font)

   clock_and_orgenda = wibox.widget {
       orgenda_counter_widget,
       clock,
       spacing = dpi(6),
       layout = wibox.layout.fixed.horizontal
   }
   clock_and_orgenda:connect_signal('mouse::enter', function() cal_show() end)
   clock_and_orgenda:connect_signal('mouse::leave', function() cal_hide() end)
   clock_and_orgenda:buttons(awful.util.table.join(
                     awful.button({         }, 1, function() cal_switch({ month = -1 }) end),
                     awful.button({         }, 2, function() cal_reset() end),
                     awful.button({         }, 3, function() cal_switch({ month =  1 }) end),
                     awful.button({         }, 4, function() cal_switch({ month =  -1 }) end),
                     awful.button({         }, 5, function() cal_switch({ month =   1 }) end),
                     awful.button({ 'Shift' }, 1, function() cal_switch({ year = -1 }) end),
                     awful.button({ 'Shift' }, 3, function() cal_switch({ year =  1 }) end),
                     awful.button({ 'Shift' }, 4, function() cal_switch({ year = -1 }) end),
                     awful.button({ 'Shift' }, 5, function() cal_switch({ year =  1 }) end)
   ))
   right_layout:add(clock_and_orgenda)

   local layout
   local left_margin_container, middle_margin_container, right_margin_container
   if beautiful.bar_style == "minimal" then
       layout = with_border {
           widget = {
               left_layout,
               {
                   tasklist_with_fallback,
                   ["content_fill_"..direction_index[shared.var.bar_position]] = true,
                   widget = wibox.container.place,
               },
               {
                   right_layout,
                   [direction_index[shared.var.bar_position] == "horizontal" and "left" or "top"] = dpi(5),
                   widget = wibox.container.margin,
               },
               layout = wibox.layout.align[direction_index[shared.var.bar_position]],
           },
           top = true
       }
   else
       middle_margin_container = with_border {
           widget = {
               tasklist_with_fallback,
               bg = beautiful.bg_normal,
               widget = wibox.container.background,
           },
           draw_empty = false,
           top = true
       }
       left_margin_container = with_border {
           widget = left_layout,
           top = true,
       }
       right_margin_container = with_border {
           id = "right_margin_container",
           widget = wibox.widget {
               right_layout,
               -- left = dpi(5),
               widget = fixed_margin
           },
           top = true,
       }
       layout = wibox.widget {
           {
               left_margin_container,
               {
                   id = "space_filler_left",
                   buttons = root_buttons,
                   ["content_fill_horizontal"] = true,
                   ["content_fill_vertical"] = true,
                   widget = fixed_place
               },
               nil,
               expand = "inside",
               layout = fixed_align[direction_index[shared.var.bar_position]]
           },
           middle_margin_container,
           {
               nil,
               {
                   id = "space_filler_right",
                   buttons = root_buttons,
                   ["content_fill_horizontal"] = true,
                   ["content_fill_vertical"] = true,
                   widget = fixed_place
               },
               right_margin_container,
               expand = "inside",
               layout = fixed_align[direction_index[shared.var.bar_position]]
           },
           expand = "outside_with_minimum",
           layout = fixed_align[direction_index[shared.var.bar_position]],
       }
   end
   scr.widgets.wibar:set_widget(layout)
   scr.widgets.wibar.left_margin_container = left_margin_container
   scr.widgets.wibar.right_margin_container = right_margin_container
   scr.widgets.wibar.middle_margin_container = middle_margin_container
end

-- Avoid nested call of reset_widgets
local reset_widgets_flag = false

local function reset_widgets()
    for _, wb in ipairs(my_wibars) do
        wb:remove()
    end
    my_wibars = {}
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
      awful.key({ "Mod4" }, "F12", function () waffle:show(nil, { anchor = "screen" }) end),
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
        local nscreen = capi.mouse.screen
        if nscreen ~= current_screen then
            if current_screen ~= nil then
                current_screen.widgets.indicator:set_context_transform_function(nil)
            end
            nscreen.widgets.indicator:set_context_transform_function({focus = true})
            -- switch active screen
            current_screen = nscreen
        end
    end
}

-- base keys and buttons
local global_keys = table_join(
   awful.key({ "Mod1" }, "Tab",
      function ()
          yams_switcher.start{}
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
   awful.key({ "Mod4" }, "Return",          function () shared.action.terminal() end),
   awful.key({ "Mod4" }, "w",               function () shared.action.web_browser() end),
   awful.key({ "Mod4" }, "e",               function () shared.action.file_manager() end),
   awful.key({ "Mod4" }, "l",               function () shared.action.screen_locker() end),
   awful.key({ "Mod4" }, "t",               function () shared.action.calendar() end),
   awful.key({ "Mod4" }, "\\",              function () shared.action.launcher() end),
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
                   return fts.get(a) < fts.get(b)
               end
            )

            for _, c in ipairs(clients) do
               c.minimized = c.orig_minimized
               c.orig_minimized = nil
            end
         end
   end),
   -- awful.key({ "Mod4" }, "q",               function ()
   --       local to_restore = true
   --       for s in capi.screen do
   --          if #s.selected_tags > 0 then
   --             to_restore = false
   --             s.orig_selected_tags = s.selected_tags
   --             awful.tag.viewnone(s)
   --          end
   --       end

   --       if not to_restore then return end
   --       for s in capi.screen do
   --          if s.orig_selected_tags ~= nil then
   --             awful.tag.viewmore(s.orig_selected_tags, s)
   --             s.orig_selected_tags = nil
   --          end
   --       end
   -- end),
   awful.key({ "Mod4", "Control" }, "r",      capi.awesome.restart),
   awful.key({ "Mod4", "Control" }, "Escape", capi.awesome.quit)
)

-- tags and layouts

shared.screen.tags = { "壹", "贰", "叁", "肆" }

for i = 1, #shared.screen.tags do
    local key = tostring(i)
    global_keys =
        table_join(
            awful.key({ "Mod4" }, tostring(i), function () switch_to_or_go_last(awful.screen.focused().tags[i]) end),
            awful.key({ "Mod4", "Control" }, tostring(i), function () awful.tag.viewtoggle(awful.screen.focused().tags[i]) end),
            awful.key({ "Mod4", "Shift" }, tostring(i), function ()
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

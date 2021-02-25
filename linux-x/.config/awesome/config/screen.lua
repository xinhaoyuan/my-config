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
machi.editor.nested_layouts["4"] = require("external.equalarea")

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
    {
        {
            widget = wibox.widget.textbox
        },
        left = beautiful.sep_median_size,
        right = beautiful.sep_median_size,
        widget = fixed_margin,
    },
    valign = "center",
    halign = "center",
    buttons = awful.util.table.join(awful.button({ }, 3, function () waffle:show(nil, { anchor = "mouse" }) end)),
    widget = fixed_place
}
fortune_widget.watch = require("watchcommand").create({"fortune", "-s"}, 300)
fortune_widget.watch:connect_signal(
   "property::output",
   function (watch)
      local raw = watch.output:gsub("\n", " "):gsub("%s+", " "):match("^%s*(.-)%s*$")
      fortune_widget.widget.widget:set_text(raw)
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
my_tray.base_size = beautiful.bar_icon_size
local bar_tray_wrapper = wibox.widget {
    my_tray,
    valign = "center",
    widget = wibox.container.place
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

local space_filler = wibox.widget {
    forced_width = beautiful.useless_gap,
    widget = wibox.container.constraint
}

local with_border = beautiful.apply_border_to_widget

local function with_top_border(widget)
    return with_border { widget = widget, top = true }
end

local space_filler_left_with_top_border = with_top_border {
    {
        {
            beautiful.sep_widget,
            forced_width = beautiful.sep_median_size,
            content_fill_vertical = true,
            content_fill_horizontal = true,
            widget = wibox.container.place
        },
        halign = "left",
        widget = fixed_place,
    },
    width = beautiful.xborder_width * 2 + beautiful.useless_gap,
    strategy = "min",
    widget = wibox.container.constraint
}

local space_filler_right_with_top_border = with_top_border {
    {
        {
            beautiful.sep_widget,
            forced_width = beautiful.sep_median_size,
            content_fill_vertical = true,
            content_fill_horizontal = true,
            widget = wibox.container.place
        },
        halign = "right",
        widget = fixed_place,
    },
    width = beautiful.xborder_width * 2 + beautiful.useless_gap,
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

    bar.left_margin_container.widget.right = beautiful.xborder_width
    bar.middle_margin_container.widget.left = beautiful.xborder_width
    bar.middle_margin_container.widget.right = beautiful.xborder_width
    bar.right_margin_container.widget.left = beautiful.xborder_width
    bar.right_margin_container.widget.widget.widget.left =
        bar.screen == capi.screen.primary and
        beautiful.xborder_radius and
        math.floor((beautiful.xborder_radius_cut and 2 - math.sqrt(2) or 1) * (beautiful.xborder_radius - beautiful.xborder_width)) or 0
end

local function set_splitted(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler})
    space_filler_right:set_children({space_filler})

    bar.left_margin_container.widget.right = beautiful.xborder_width
    bar.middle_margin_container.widget.left = beautiful.xborder_width
    bar.middle_margin_container.widget.right = beautiful.xborder_width
    bar.right_margin_container.widget.left = beautiful.xborder_width
    bar.right_margin_container.widget.widget.widget.left =
        bar.screen == capi.screen.primary and
        beautiful.xborder_radius and
        math.floor((beautiful.xborder_radius_cut and 2 - math.sqrt(2) or 1) * (beautiful.xborder_radius - beautiful.xborder_width)) or 0
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

-- Next TODO item

local next_todo_widget = wibox.widget {
    widget = wibox.widget.textbox
}
local function update_next_todo()
    local next_todo = {}
    local date = os.date("*t")
    local time = os.time(date)
    if orgenda.data.items ~= nil then
        for _, item in ipairs(orgenda.data.items) do
            if item.timestamp ~= nil and item.has_time then
                print(time, item.timestamp)
                if item.timestamp > time and (#next_todo == 0 or next_todo[1].timestamp >= item.timestamp) then
                    if #next_todo > 0 and next_todo[1].timestamp > item.timestamp then
                        next_todo = {}
                    end
                    next_todo[#next_todo + 1] = item
                end
            end
        end
    end
    local ndate = #next_todo > 0 and os.date("*t", next_todo[1].timestamp)
    if #next_todo == 0 then
        next_todo_widget.markup = ""
    else
        if ndate.year == date.year and ndate.month == date.month and ndate.day == date.day then
            next_todo_widget.markup = "!"..os.date("%H<b>%M</b>", next_todo[1].timestamp)
        else
            next_todo_widget.markup = ""
        end
    end
end

-- Calendar popup

local today = os.date("*t")
local last_mid_update_timestamp = nil
local active_dates = {}
local cal_widget = wibox.widget {
    date = os.date('*t'),
    font = beautiful.font_mono,
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
    call_now = true,
    callback = function()
        local new_today = os.date("*t")
        if today.day ~= new_today.day then
            today = new_today
            cal_widget:emit_signal("widget::layout_changed")
        end

        local timestamp = os.clock()
        if last_mid_update_timestamp == nil or
            timestamp - last_mid_update_timestamp > 300 then
            last_mid_update_timestamp = timestamp
            update_next_todo()
        end
    end
}
orgenda.data:connect_signal(
    "update",
    function ()
        active_dates = {}
        for _, item in ipairs(orgenda.data.items) do
            if item.timestamp then
                local date = os.date("*t", item.timestamp)
                local y = active_dates[date.year] or {}
                local m = y[date.month] or {}
                m[date.day] = true
                y[date.month] = m
                active_dates[date.year] = y
            end
        end
        update_next_todo()
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
                        {
                            orgenda.widget {
                                width = dpi(240),
                                indent_width = dpi(26),
                                item_margin = beautiful.sep_small_size,
                            },
                            halign = "left",
                            widget = wibox.container.place
                        },
                        draw_empty = false,
                        top = beautiful.sep_big_size,
                        widget = fixed_margin,
                    },
                    bgimage = function(context, cr, width, height)
                        height = beautiful.sep_big_size
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
            image = gcolor.recolor_image(icons.calendar_todo, beautiful.fg_normal),
            forced_height = beautiful.bar_icon_size,
            forced_width = beautiful.bar_icon_size,
            widget = wibox.widget.imagebox,
        },
        valign = "center",
        widget = wibox.container.place
    },
    orgenda_counter_text_widget,
    layout = wibox.layout.fixed.horizontal
}

orgenda.data:connect_signal(
    "update",
    function (_, path, items)
        if #orgenda.data.items > 0 then
            orgenda_counter_widget.visible = true
            local high = 0
            local mid = 0
            local low = 0
            for _, item in ipairs(orgenda.data.items) do
                if item.priority == 3 then high = high + 1
                elseif item.priority == 2 then mid = mid + 1
                else low = low + 1
                end
            end
            if high > 0 then high = "<span foreground='"..beautiful.special_normal.."'><b>"..tostring(high).."</b></span>" else high = "" end
            if mid > 0 then mid = "<b>"..tostring(mid).."</b>" else mid = "" end
            if low > 0 then low = tostring(low) else low = "" end
            orgenda_counter_text_widget.markup = high..((#high > 0 and #mid + #low > 0) and "/" or "")..mid..((#mid > 0 and #low > 0) and "/" or "")..low
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
       style = {
           font = "DejaVu Sans 10",
       },
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
             beautiful.xborder_width,
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
      spacing        = beautiful.sep_median_size,
      spacing_widget = beautiful.sep_widget,
      layout         = wibox.layout.fixed[direction_index[shared.var.bar_position]]
   }

   if scr == primary_screen then
       right_layout:add(bar_tray_wrapper)
   end

   local clock
   if direction_index[shared.var.bar_position] == "horizontal" then
       clock = wibox.widget.textclock("<span color='" .. beautiful.border_focus .. "'>%m<b>%d</b></span> %H<b>%M</b>")
   else
       clock = wibox.widget.textclock("<span color='" .. beautiful.border_focus .. "'>%m\n<b>%d</b></span>\n%H\n<b>%M</b>")
   end
   clock.align = "center"
   clock:set_font(beautiful.font)

   clock_and_orgenda = wibox.widget {
       {
           clock,
           left = beautiful.sep_small_size,
           right = beautiful.sep_small_size,
           widget = fixed_margin,
       },
       {
           orgenda_counter_widget,
           right = beautiful.sep_small_size,
           draw_empty = false,
           widget = fixed_margin,
       },
       {
           next_todo_widget,
           right = beautiful.sep_small_size,
           draw_empty = false,
           widget = fixed_margin,
       },
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
                   [direction_index[shared.var.bar_position] == "horizontal" and "left" or "top"] = beautiful.sep_median_size,
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
               -- left = beautiful.sep_median_size,
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
   awful.key({ "Mod4" }, ".", function ()
           shared.client.start_switcher(capi.client.focus, false)
   end),

   awful.key({ "Mod4" }, "Tab", function ()
           shared.client.start_switcher(capi.client.focus, true)
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
   awful.key({ "Mod4" }, "r",               function () shared.action.launcher() end),
   awful.key({ "Mod4" }, "F1",              function () shared.action.terminal_session{ name = "F1" } end),
   awful.key({ "Mod4" }, "F2",              function () shared.action.terminal_session{ name = "F2" } end),
   awful.key({ "Mod4" }, "F3",              function () shared.action.terminal_session{ name = "F3" } end),
   awful.key({ "Mod4" }, "F4",              function () shared.action.terminal_session{ name = "F4" } end),
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

-- shared.screen.tags = { "壹", "贰", "叁", "肆" }
shared.screen.tags = { "☱", "☲", "☳", "☴" }

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

local gears = require("gears")
local gsurf = require("gears.surface")
local cairo = require("lgi").cairo
function shared.screen.xkcd(num)
    local cmd = {"xkcd_fetcher.py", "--recolor="..beautiful.fg_normal.."-"..beautiful.bg_normal}
    if num ~= nil then
        cmd[#cmd + 1] = "-n"
        cmd[#cmd + 1] = tostring(num)
    end
    awful.spawn.easy_async(cmd, function (stdout, stderr, reason, code)
            if reason ~= "exit" or code ~= 0 then
                print("Failed to fetch xkcd. reason: "..reason..", code: "..tostring(code))
                print("Stdout:", stdout)
                print("Stderr:", stderr)
                return
            else
                print("Fetched image. stderr:", stderr)
            end

            local filename = stdout:match("^(.*)\n")

            local surf = gsurf.load_uncached(filename)
            local pattern = cairo.Pattern.create_for_surface(surf)
            -- pattern:set_extend("REPEAT")
            local w, h = gsurf.get_size(surf)
            local ratio = 0.6

            for s in screen do
                local geom, cr = gears.wallpaper.prepare_context(s)
                local scale, target_w, target_h
                if geom.width / w < geom.height / h then
                    scale = ratio * geom.width / w
                else
                    scale = ratio * geom.height / h
                end
                if scale > dpi(1) then scale = dpi(1) end
                if scale > 1 then scale = math.floor(scale + 0.5) end
                target_w = scale * w
                target_h = scale * h

                cr:set_operator("SOURCE")
                cr:set_source(gcolor(beautiful.bg_normal))
                cr:paint()

                -- print("gw", geom.width, "gh", geom.height, "w", w, "h", h, "tw", target_w, "th", target_h, "scale", scale)
                cr:translate(geom.width / 2 - target_w / 2, geom.height / 2 - target_h / 2)
                cr:rectangle(0, 0, target_w, target_h)
                cr:clip()
                cr:scale(scale, scale)

                cr:set_operator("OVER")
                if scale == math.floor(scale) then pattern:set_filter("NEAREST") else pattern:set_filter("BILINEAR") end
                cr:set_source(pattern)
                cr:paint()
            end

            surf:finish()
            os.remove(filename)
    end)
end

return nil

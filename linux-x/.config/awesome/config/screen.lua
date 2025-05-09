local capi = {
   awesome = awesome,
   screen = screen,
   mouse = mouse,
   client = client,
   root = root,
}

local prefix = (...):match("(.-)[^%.]+$")
local shared = require(prefix .. "shared")
shared.screen = {}
local cwidget = require((...):match("(.-)[^%.]+$") .. "widget")

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
local recolor = require("recolor")
local debug_container = require("debug_container")
local compactor = require("compactor")
local tasklist = require("config.tasklist")
local taglist = require("config.taglist")
local prism = require("prism")
local fts = require("hotpot").focus_timestamp
local aux = require("aux")
local icons = require("icons")
local orgenda = require("orgenda")
local tapdancer = require("tapdancer")
local notix = require("notix")
notix.config.filter = function (notif)
    return notif.app_name ~= "pasystray"
end
require("manage_ticket")


-- helper functions

local table_join = awful.util.table.join
local delayed = gtimer.delayed_call

local function go_screen_by_direction(dir, with_object)
    if with_object == "client" then
        local c = capi.client.focus
        if not c then return end
        awful.screen.focus_bydirection(dir, c.screen)
        c:move_to_screen(capi.mouse.screen.index)
        c:emit_signal("request::activate", "mouse.resize", {raise = true})
    elseif with_object == "tag" then
        local s = awful.screen.focused()
        if #s.selected_tags == 1 then
            local t = s.selected_tag
            awful.screen.focus_bydirection(dir)
            local s2 = awful.screen.focused()
            if s ~= s2 then
                local t2 = s2.selected_tag or s2.tags[t.index]
                taglist.swap_tags(t, t2)
                t:view_only()
                t2:view_only()
            end
        end
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
local tabber = require("tabber")
local xlayout = tabber{
    layout = require("external.equalarea"),
    bar_size = beautiful.bar_height + 2 * beautiful.xborder_width,
    bar_bg = "#00000000",
    bar_min_clients = 2,
    set_bar_clients = function (bar, clients)
        if not bar.widget then
            bar.container = wibox.widget{
                spacing = beautiful.sep_median_size,
                layout = wibox.layout.flex.horizontal,
            }
            bar.widget = wibox.widget{
                nil,
                beautiful.apply_border_to_widget{
                    widget = wibox.widget{
                        bar.container,
                        left = beautiful.xborder_indent,
                        right = beautiful.xborder_indent,
                        widget = wibox.container.margin,
                    },
                    top = true,
                    left = true,
                    right = true,
                    bottom = true,
                },
                nil,
                expand = "outside",
                layout = wibox.layout.align.horizontal,
            }
        end

        if #clients == 0 then
            bar.visible = false
        else
            bar.visible = true
            bar.container:reset()
            table.sort(clients, function (a, b) return a.window < b.window end)
            for _, c in ipairs(clients) do
                bar.container:add(
                    wibox.widget {
                        {
                            widget = awful.widget.clienticon(c),
                        },
                        {
                            image = beautiful.client_default_icon,
                            widget = masked_imagebox,
                        },
                        widget = fallback,
                    }
                )
            end
        end
    end,
}
machi.editor.nested_layouts["4"] = xlayout

local yggdrasil = safe_require("yggdrasil")
if yggdrasil then
    yggdrasil.set_icon()
end

local alayout = require("awful.layout")
local layouts = {
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
    alayout.suit.corner.ne,
    alayout.suit.corner.sw,
    alayout.suit.corner.se,
}
if yggdrasil then table.insert(layouts, 2, yggdrasil.create_root{}) end
alayout.layouts = layouts

-- Define the tag list upfront for keybindings

local root_buttons = awful.util.table.join(
    awful.button({ }, 3, nil, function () capi.awesome.emit_signal("show_main_waffle", {anchor = "mouse"}) end)
)
awful.mouse.append_global_mousebinding(root_buttons)

local fortune_widget = wibox.widget{
    {
        {
            {
                id = "contents",
                widget = wibox.widget.textbox,
            },
            widget = compactor,
        },
        left = beautiful.sep_median_size,
        right = beautiful.sep_median_size,
        widget = fixed_margin,
    },
    valign = "center",
    halign = "center",
    buttons = awful.util.table.join(awful.button({ }, 3, nil, function () capi.awesome.emit_signal("show_main_waffle", {anchor = "mouse"}) end)),
    widget = fixed_place
}
fortune_widget.watch = require("watchcommand").create({"fortune", "-s"}, 300)
fortune_widget.watch:connect_signal(
   "property::output",
   function (watch)
      local raw = watch.output:gsub("\n", " "):gsub("%s+", " "):match("^%s*(.-)%s*$")
      fortune_widget:get_children_by_id("contents")[1]:set_text(raw)
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

local my_bars = {}
local my_tray
my_tray = wibox.widget.systray()
local bar_tray_wrapper = wibox.widget {
    {
        my_tray,
        strength = 0.5,
        widget = recolor,
    },
    valign = "bottom",
    widget = wibox.container.place,
}

function shared.screen.detach_tray_widget()
    bar_tray_wrapper.widget = nil
    return my_tray
end

function shared.screen.attach_tray_widget()
    bar_tray_wrapper.widget = my_tray
end

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

local space_filler
local space_filler_left_with_top_border
local space_filler_right_with_top_border

local with_border = beautiful.apply_border_to_widget

local function with_top_border(widget)
    return with_border { widget = widget, [top_index[shared.vars.bar_position]] = true }
end

local function set_expanded(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler_left_with_top_border})
    space_filler_right:set_children({space_filler_right_with_top_border})

    local container
    container = bar:get_children_by_id("left_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.left_margin_expanded)
    end
    container = bar:get_children_by_id("middle_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.middle_margin_expanded)
    end
    container = bar:get_children_by_id("right_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.right_margin_expanded)
    end
end

local function set_splitted(bar)
    local space_filler_left = bar.widget:get_children_by_id("space_filler_left")[1]
    local space_filler_right = bar.widget:get_children_by_id("space_filler_right")[1]
    space_filler_left:set_children({space_filler})
    space_filler_right:set_children({space_filler})

    local container
    container = bar:get_children_by_id("left_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.left_margin_splitted)
    end
    container = bar:get_children_by_id("middle_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.middle_margin_splitted)
    end
    container = bar:get_children_by_id("right_margin_holder")
    if container and #container > 0 then
        container[1]:set_widget(bar.right_margin_splitted)
    end
end

capi.awesome.connect_signal(
    "tasklist::update::before",
    function (s)
        if beautiful.bar_style == "split" then
            set_splitted(s.widgets.bar)
            return
        elseif beautiful.bar_style == "simple" then
            set_expanded(s.widgets.bar)
            return
        elseif beautiful.bar_style ~= "auto" then
            print("Expecting auto bar_style but got", beautiful.bar_style)
        end

        local has_maximized = false
        for _, c in ipairs(s.clients) do
            has_maximized = has_maximized or c.maximized
        end
        if has_maximized then
            set_expanded(s.widgets.bar)
        else
            set_splitted(s.widgets.bar)
        end
    end
)

-- Next TODO item

local next_todo_counter = wibox.widget.textbox()
local next_todo_widget_container = wibox.container.background()
local function update_next_todo()
    local next_todo = {}
    local date = os.date("*t")
    local time = os.time(date)
    if orgenda.data.items ~= nil then
        for _, item in ipairs(orgenda.data.items) do
            if item.timestamp ~= nil and item.has_time and not item.done then
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
        next_todo_counter.markup = ""
        next_todo_widget_container.visible = false
    else
        if ndate.year == date.year and ndate.month == date.month and ndate.day == date.day then
            next_todo_counter.markup = os.date("%H:%M", next_todo[1].timestamp)
            next_todo_widget_container.visible = true
        else
            next_todo_counter.markup = ""
            next_todo_widget_container.visible = false
        end
    end
end
local last_mid_update_timestamp = nil
gtimer {
    timeout = 10,
    autostart = true,
    call_now = true,
    callback = function()
        local timestamp = os.clock()
        if last_mid_update_timestamp == nil or
            timestamp - last_mid_update_timestamp > 300 then
            last_mid_update_timestamp = timestamp
            update_next_todo()
        end
    end
}

-- Orgenda
local orgenda_counter_data = {}
local orgenda_counter_text_widget = wibox.widget{
    pickers = {
        markup = prism.picker.wrap_raw{
            function (context)
                local high = orgenda_counter_data.high or 0
                local mid = orgenda_counter_data.mid or 0
                local low = orgenda_counter_data.low or 0
                if high > 0 then high = "<span color='"..(context.highlighted and beautiful.special_focus or beautiful.special_normal).."'><b>"..tostring(high).."</b></span>" else high = "" end
                if mid > 0 then mid = "<b>"..tostring(mid).."</b>" else mid = "" end
                if low > 0 then low = tostring(low) else low = "" end
                return high..((#high > 0 and #mid + #low > 0) and "!" or "")..mid..((#mid > 0 and #low > 0) and "/" or "")..low
            end},
    },
    widget = prism.widget.textbox,
}
local orgenda_counter_widget_container = wibox.container.background()

orgenda.data:connect_signal(
    "property::items",
    function (_, path, items)
        update_next_todo()
        if #orgenda.data.items > 0 then
            local high = 0
            local mid = 0
            local low = 0
            for _, item in ipairs(orgenda.data.items) do
                if not item.done then
                    if item.priority == 3 then high = high + 1
                    elseif item.priority == 2 then mid = mid + 1
                    else low = low + 1
                    end
                end
            end
            orgenda_counter_data = {
                high = high,
                mid = mid,
                low = low,
            }
            orgenda_counter_text_widget:emit_signal("prism::widget_changed")
            orgenda_counter_text_widget:emit_signal("widget::layout_changed")
            orgenda_counter_text_widget:emit_signal("widget::redraw_needed")
            orgenda_counter_widget_container.visible = high + mid + low > 0
        else
            orgenda_counter_widget_container.visible = false
        end
    end
)

-- Notix

local notix_counter_widget_container = wibox.container.background()
notix_counter_widget_container.visible = false
capi.awesome.connect_signal(
    "notix::on_counter_change", function (counter)
        notix_counter_widget_container.visible = counter > 0
    end)

local waffle_indicator = wibox.widget{
    image = icons.waffle,
    widget = masked_imagebox,
    visible = false,
}

capi.awesome.connect_signal("waffle.show", function () if not waffle:autohide() then waffle_indicator.visible = true end end)
capi.awesome.connect_signal("waffle.hide", function () waffle_indicator.visible = false end)

local function setup_screen(scr)
   scr.mypromptbox = awful.widget.prompt()

   scr.widgets = {}
   scr.actions = {}
   local tasklist = tasklist.create(scr)
   scr.widgets.tasklist = tasklist
   scr.tasklist_clients = function()
       local clients = scr.all_clients
       local ticket = {}
       for _, c in ipairs(clients) do
           if c.cgroup then
               local old_ticket = ticket[c.cgroup]
               ticket[c.cgroup] = (old_ticket == nil) and c.manage_ticket or math.min(old_ticket, c.manage_ticket)
           end
       end
       table.sort(clients,
                  function (a, b)
                      -- local a_iconized = a.cgroup == nil and a.tasklist_icon_only == true
                      -- local b_iconized = b.cgroup == nil and b.tasklist_icon_only == true
                      -- if a_iconized ~= b_iconized then return b_iconized end
                      -- Minimized windows appear at last
                      if (a.cgroup and a.cgroup.current_client or a).minimized ~= (b.cgroup and b.cgroup.current_client or b).minimized then
                          return (b.cgroup and b.cgroup.current_client or b).minimized
                      end
                      --[[
                      -- Maximized windows first
                      if (a.cgroup and a.cgroup.current_client or a).maximized ~= (b.cgroup and b.cgroup.current_client or b).maximized then
                          return (a.cgroup and a.cgroup.current_client or a).maximized
                      end
                      ]]--
                      local a_ticket = a.cgroup and ticket[a.cgroup] or a.manage_ticket
                      local b_ticket = b.cgroup and ticket[b.cgroup] or b.manage_ticket
                      if a_ticket == b_ticket then
                          return a.manage_ticket < b.manage_ticket
                      else
                          return a_ticket < b_ticket
                      end
                  end
                 )
       return clients
   end
   local tasklist_with_fallback = {
       tasklist,
       {
           fortune_widget,
           direction = direction_index[shared.vars.bar_position] == "horizontal" and "north" or "west",
           widget = wibox.container.rotate
       },
       widget = fallback,
   }

   scr.widgets.tag_list = taglist.create(scr)
   scr.widgets.bar_placeholder = awful.wibar({
         screen = scr,
         ontop = false,
         bg = "#00000000",
         [size_index[shared.vars.bar_position]] =
             beautiful.bar_height +
             beautiful.decorator_border_width +
             beautiful.decorator_padding_width,
         position = shared.vars.bar_position,
         border_width = 0,
         input_passthrough = true,
   })
   my_bars[#my_bars + 1] = scr.widgets.bar_placeholder
   scr.widgets.bar = wibox{
       screen = scr,
       ontop = false,
       fg = beautiful.fg_normal,
       bg = "#00000000",
       border_width = 0,
       opacity = 1,
       [size_index[shared.vars.bar_position]] =
           beautiful.bar_height +
           beautiful.decorator_shade_width +
           beautiful.decorator_border_width +
           beautiful.decorator_padding_width,
       cursor = "cross",
       visible = true,
       type = "dock",
   }
   my_bars[#my_bars + 1] = scr.widgets.bar

   -- local
   space_filler = wibox.widget {
       [direction_index[shared.vars.bar_position] == "horizontal" and "forced_width" or "forced_height"] = beautiful.useless_gap,
       widget = wibox.container.constraint
   }

   -- local
   space_filler_left_with_top_border = with_top_border {
       {
           {
               beautiful.sep_widget,
               [direction_index[shared.vars.bar_position] == "horizontal" and "forced_width" or "forced_height"] = beautiful.sep_median_size,
               content_fill_vertical = true,
               content_fill_horizontal = true,
               widget = wibox.container.place
           },
           [direction_index[shared.vars.bar_position] == "horizontal" and "halign" or "valign"] = direction_index[shared.vars.bar_position] == "horizontal" and "left" or "top",
           widget = fixed_place,
       },
       [direction_index[shared.vars.bar_position] == "horizontal" and "width" or "height"] = beautiful.xborder_width * 2 + beautiful.useless_gap,
       strategy = "min",
       widget = wibox.container.constraint
   }

   -- local
   space_filler_right_with_top_border = with_top_border {
       {
           {
               beautiful.sep_widget,
               [direction_index[shared.vars.bar_position] == "horizontal" and "forced_width" or "forced_height"] = beautiful.sep_median_size,
               content_fill_vertical = true,
               content_fill_horizontal = true,
               widget = wibox.container.place
           },
           [direction_index[shared.vars.bar_position] == "horizontal" and "halign" or "valign"] = direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom",
           widget = fixed_place,
       },
       [direction_index[shared.vars.bar_position] == "horizontal" and "width" or "height"] = beautiful.xborder_width * 2 + beautiful.useless_gap,
       strategy = "min",
       widget = wibox.container.constraint
   }

   local left_layout = wibox.layout.fixed[direction_index[shared.vars.bar_position]]()
   local layoutbox = awful.widget.layoutbox{screen = scr}
   masked_imagebox.convert(layoutbox.imagebox)
   scr.widgets.indicator = wibox.widget{
       {
           {
               waffle_indicator,
               layoutbox,
               widget = fallback,
           },
           pickers = {
               fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
               prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
           },
           widget = prism.container.background,
       },
       widget = prism.layer,
   }
   scr.widgets.indicator:buttons(
      awful.util.table.join(
          awful.button({ }, 1, nil, function () capi.awesome.emit_signal("show_main_waffle", {anchor = "mouse"}) end),
          -- awful.button({ }, 3, nil, function ()
          --         if capi.client.focus ~= nil then
          --             capi.awesome.emit_signal("show_client_waffle", capi.client.focus, "mouse")
          --         end
          -- end),
          awful.button({ }, 4, nil, function () awful.layout.inc( 1) end),
          awful.button({ }, 5, nil, function () awful.layout.inc(-1) end)))
   scr.widgets.indicator:connect_signal(
       'mouse::enter', function()
           local waffle_scr = waffle:get_screen()
           if waffle:autohide() or waffle_scr == nil or waffle_scr ~= scr then
               local anchor = {}
               if direction_index[shared.vars.bar_position] == "horizontal" then
                   anchor.x = scr.geometry.x
                   anchor.y = shared.vars.bar_position == "top" and scr.geometry.y or scr.geometry.y + scr.geometry.height
               else
                   anchor.x = shared.vars.bar_position == "left" and scr.geometry.x or scr.geometry.x + scr.geometry.width
                   anchor.y = scr.geometry.y
               end
               local locking_callback = function ()
                   if capi.mouse.current_widgets then
                       for _, w in ipairs(capi.mouse.current_widgets) do
                           if w == scr.widgets.indicator then return true end
                       end
                   end
               end
               capi.awesome.emit_signal(
                   "show_main_waffle", {
                       anchor = anchor,
                       screen = scr,
                       autohide = 0.5,
                       autohide_locking_callback = locking_callback,
                   })
           end
       end)
   scr.widgets.indicator:connect_signal(
       'mouse::leave', function()
           waffle:autohide_delayed_check(true)
       end)
   left_layout:add(scr.widgets.indicator)
   left_layout:add(scr.widgets.tag_list)
   left_layout:add(scr.mypromptbox)
   local right_layout = wibox.widget {
      -- spacing        = beautiful.sep_median_size,
      -- spacing_widget = beautiful.sep_widget,
      layout         = wibox.layout.fixed[direction_index[shared.vars.bar_position]]
   }

   right_layout:add(beautiful.with_separator{
                        widget = {
                            {
                                cwidget.playerctl_widget,
                                strength = 0.5,
                                widget = recolor,
                            },
                            width = dpi(150),
                            widget = wibox.container.constraint,
                        },
                        separator_side = direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom",
                        separator_size = beautiful.sep_median_size,
                    })

   if scr == primary_screen then
       right_layout:add(beautiful.with_separator{
                            widget = bar_tray_wrapper,
                            separator_side = direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom",
                            separator_size = beautiful.sep_median_size,
                        })
   end

   local clock
   if direction_index[shared.vars.bar_position] == "horizontal" then
       clock = wibox.widget{
           {
               {
                   format = "%m/%d",
                   align = "center",
                   widget = wibox.widget.textclock,
               },
               pickers = {
                   fg = prism.picker.beautiful{"minor_", prism.picker.highlighted_switcher},
               },
               widget = prism.container.background,
           },
           {
               format = "%H:%M",
               align = "center",
               widget = wibox.widget.textclock,
           },
           spacing = beautiful.bar_rows == 1 and beautiful.sep_small_size or 0,
           expand = true,
           layout = wibox.layout.grid[direction_index[shared.vars.bar_position]](beautiful.bar_rows),
       }
   else
       clock = wibox.widget{
           {
               {
                   format = beautiful.bar_rows == 1 and "%m\n%d" or "%m/%d",
                   align = "center",
                   widget = wibox.widget.textclock,
               },
               pickers = {
                   fg = prism.picker.beautiful{"minor_", prism.picker.highlighted_switcher},
               },
               widget = prism.container.background,
           },
           {
               format = beautiful.bar_rows == 1 and "%H\n%M" or "%H:%M",
               align = "center",
               widget = wibox.widget.textclock,
           },
           spacing = beautiful.sep_small_size,
           layout = wibox.layout.fixed.vertical,
       }
   end
   orgenda_counter_widget_container.children = {
       wibox.widget{
           {
               {
                   image = icons.calendar_todo,
                   forced_height = beautiful.bar_height / beautiful.bar_rows,
                   forced_width = beautiful.bar_height / beautiful.bar_rows,
                   widget = masked_imagebox,
               },
               widget = wibox.container.place
           },
           {
               orgenda_counter_text_widget,
               widget = wibox.container.place
           },
           expand = true,
           [direction_index[shared.vars.bar_position].."_homogeneous"] = false,
           layout = wibox.layout.grid[direction_index[shared.vars.bar_position]](beautiful.bar_rows),
       }
   }
   notix_counter_widget_container.children = {
       wibox.widget{
           {
               {
                   image = icons.notification,
                   forced_height = beautiful.bar_height / beautiful.bar_rows,
                   forced_width = beautiful.bar_height / beautiful.bar_rows,
                   widget = masked_imagebox,
               },
               widget = wibox.container.place,
           },
           {
               notix.counter_widget,
               widget = wibox.container.place,
           },
           expand = true,
           [direction_index[shared.vars.bar_position].."_homogeneous"] = false,
           layout = wibox.layout.grid[direction_index[shared.vars.bar_position]](beautiful.bar_rows),
       }
   }
   next_todo_widget_container.children = {
       wibox.widget{
           {
               {
                   image = icons.alert,
                   forced_height = beautiful.bar_height / beautiful.bar_rows,
                   forced_width = beautiful.bar_height / beautiful.bar_rows,
                   widget = masked_imagebox,
               },
               widget = wibox.container.place,
           },
           next_todo_counter,
           expand = true,
           [direction_index[shared.vars.bar_position].."_homogeneous"] = false,
           layout = wibox.layout.grid[direction_index[shared.vars.bar_position]](beautiful.bar_rows),
       }
   }
   local clock_area = wibox.widget{
       {
           {
               {
                   clock,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "left" or "top"] = beautiful.sep_small_size,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom"] = beautiful.sep_small_size,
                   widget = fixed_margin,
               },
               {
                   orgenda_counter_widget_container,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom"] = beautiful.sep_small_size,
                   draw_empty = false,
                   widget = fixed_margin,
               },
               {
                   next_todo_widget_container,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom"] = beautiful.sep_small_size,
                   draw_empty = false,
                   widget = fixed_margin,
               },
               {
                   notix_counter_widget_container,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom"] = beautiful.sep_small_size,
                   draw_empty = false,
                   widget = fixed_margin,
               },
               layout = wibox.layout.fixed[direction_index[shared.vars.bar_position]],
           },
           draw_pickers = {
               fg = prism.picker.beautiful{"fg_", prism.picker.highlighted_switcher},
               prism.picker.list{"bg", prism.picker.beautiful{"bg_", prism.picker.branch{"highlighted", "focus"}}},
           },
           widget = prism.container.background,
       },
       context_transformation = {highlighted = false},
       widget = prism.layer,
   }
   local clock_area_focused
   function scr.actions.set_clock_area_focus(f)
       clock_area.context_transformation = {highlighted = f}
       clock_area_focused = f
   end
   local clock_corner_anchor = {}
   if direction_index[shared.vars.bar_position] == "horizontal" then
       clock_corner_anchor.x = scr.geometry.x + scr.geometry.width
       clock_corner_anchor.y = shared.vars.bar_position == "top" and scr.geometry.y or scr.geometry.y + scr.geometry.height
   else
       clock_corner_anchor.x = shared.vars.bar_position == "left" and scr.geometry.x or scr.geometry.x + scr.geometry.width
       clock_corner_anchor.y = scr.geometry.y + scr.geometry.height
   end
   clock_area:buttons(
       awful.util.table.join(
           awful.button({         }, 1, nil, function()
                            if not clock_area_focused then
                                gtimer.delayed_call(
                                    function()
                                        capi.awesome.emit_signal("toggle_calendar_waffle", { anchor = clock_corner_anchor })
                                        if not waffle:is_in_view(nil) then
                                            scr.actions.set_clock_area_focus(true)
                                        end
                                    end
                                )
                            end
                        end),
           awful.button({         }, 2, nil, function() notix.remove_unpinned() end)
       )
   )
   clock_area:connect_signal(
       'mouse::enter', function()
           local waffle_scr = waffle:get_screen()
           if waffle:autohide() or waffle_scr == nil or waffle_scr ~= scr then
               local locking_callback = function ()
                   if capi.mouse.current_widgets then
                       for _, w in ipairs(capi.mouse.current_widgets) do
                           if w == clock_area then return true end
                       end
                   end
               end
               capi.awesome.emit_signal(
                   "show_calendar_waffle", {
                       anchor = clock_corner_anchor,
                       screen = scr,
                       autohide = 0.5,
                       autohide_locking_callback = locking_callback,
                   })
           end
       end)
   clock_area:connect_signal(
       'mouse::leave', function()
           waffle:autohide_delayed_check(true)
       end)
   right_layout:add(clock_area)
   scr.widgets.clock_area = clock_area

   local layout
   local left_margin_container, middle_margin_container, right_margin_container
   if beautiful.bar_style == "minimal" then
       layout = with_border {
           widget = {
               left_layout,
               {
                   tasklist_with_fallback,
                   ["content_fill_"..direction_index[shared.vars.bar_position]] = true,
                   widget = wibox.container.place,
               },
               {
                   right_layout,
                   [direction_index[shared.vars.bar_position] == "horizontal" and "left" or "top"] = beautiful.sep_median_size,
                   widget = wibox.container.margin,
               },
               layout = fixed_align[direction_index[shared.vars.bar_position]],
           },
           [top_index[shared.vars.bar_position]] = true
       }
   else
       scr.widgets.bar.middle_margin_expanded = with_border {
           widget = tasklist_with_fallback,
           draw_empty = false,
           [top_index[shared.vars.bar_position]] = true
       }
       scr.widgets.bar.left_margin_expanded = with_border {
           widget = left_layout,
           [top_index[shared.vars.bar_position]] = true,
       }
       scr.widgets.bar.right_margin_expanded = with_border {
           widget = wibox.widget {
               right_layout,
               -- left = beautiful.sep_median_size,
               widget = fixed_margin
           },
           [top_index[shared.vars.bar_position]] = true,
       }

       scr.widgets.bar.middle_margin_splitted = with_border {
           widget = tasklist_with_fallback,
           draw_empty = false,
           [top_index[shared.vars.bar_position]] = true, [left_index[shared.vars.bar_position]] = true, [right_index[shared.vars.bar_position]] = true,
       }
       scr.widgets.bar.left_margin_splitted = with_border {
           widget = left_layout,
           [top_index[shared.vars.bar_position]] = true, [direction_index[shared.vars.bar_position] == "horizontal" and "right" or "bottom"] = true,
       }
       scr.widgets.bar.right_margin_splitted = with_border {
           widget = wibox.widget {
               right_layout,
               -- left = scr == primary_screen and beautiful.sep_median_size or nil,
               widget = fixed_margin
           },
           [top_index[shared.vars.bar_position]] = true, [direction_index[shared.vars.bar_position] == "horizontal" and "left" or "top"] = true,
       }

       layout = wibox.widget {
           {
               {
                   id = "left_margin_holder",
                   scr.widgets.bar.left_margin_expanded,
                   widget = wibox.container.background,
               },
               {
                   id = "space_filler_left",
                   buttons = root_buttons,
                   ["content_fill_horizontal"] = true,
                   ["content_fill_vertical"] = true,
                   widget = fixed_place
               },
               nil,
               expand = "inside",
               layout = fixed_align[direction_index[shared.vars.bar_position]]
           },
           {
               id = "middle_margin_holder",
               scr.widgets.bar.middle_margin_expanded,
               widget = wibox.container.background,
           },
           {
               nil,
               {
                   id = "space_filler_right",
                   buttons = root_buttons,
                   ["content_fill_horizontal"] = true,
                   ["content_fill_vertical"] = true,
                   widget = fixed_place
               },
               {
                   id = "right_margin_holder",
                   scr.widgets.bar.right_margin_expanded,
                   widget = wibox.container.background,
               },
               expand = "inside",
               layout = fixed_align[direction_index[shared.vars.bar_position]]
           },
           expand = "outside_with_minimum",
           layout = fixed_align[direction_index[shared.vars.bar_position]],
       }
   end
   (awful.placement[shared.vars.bar_position] + awful.placement["maximize_"..shared.direction_index[shared.vars.bar_position].."ly"])(
       scr.widgets.bar, {attach = true})
   scr.widgets.bar.widget = layout
   scr.widgets.bar.visible = true
end

capi.screen.connect_signal("list", function() waffle:autohide_delayed_check() end)

-- Avoid nested call of reset_widgets
local reset_widgets_flag = false

local function reset_widgets()
    for _, wb in ipairs(my_bars) do
        if wb.remove then
            wb:remove()
        else
            wb.visible = false
        end
    end
    my_bars = {}
    current_screen = nil
    primary_screen = capi.screen.primary

    my_tray.horizontal = direction_index[shared.vars.bar_position] == "horizontal"
    my_tray.base_size = beautiful.bar_height / beautiful.bar_rows

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

local function bound_client_geometry(c, bound)
    local cgeo = c:geometry()
    local nx = math.max(bound.x, math.min(cgeo.x, bound.x + bound.width - cgeo.width))
    local ny = math.max(bound.y, math.min(cgeo.y, bound.y + bound.height - cgeo.height))
    local nw = math.min(cgeo.width, bound.x + bound.width - nx)
    local nh = math.min(cgeo.height, bound.y + bound.height - ny)
    if nx ~= cgeo.x or ny ~= cgeo.y or nw ~= cgeo.width or nh ~= cgeo.height then
        c:geometry{x = nx, y = ny, width = nw, height = nh}
    end
end

capi.client.connect_signal("manage", function (c) bound_client_geometry(c, c.screen.geometry) end)

local function fix_client_geometries()
    local clients = {}
    for _, c in ipairs(capi.client.get()) do
        if not c.minimized and c.maximized then
            c.maximized = false
            table.insert(clients, c)
        end

        bound_client_geometry(c, c.screen.geometry)
    end

    delayed(function ()
                for _, c in ipairs(clients) do
                    c.maximized = true
                end
            end)
end

local is_fixing_client_geometries = false
local function schedule_fix_client_geometries()
    if is_fixing_client_geometries then return end
    is_fixing_client_geometries = true
    delayed(function ()
                is_fixing_client_geometries = false
                fix_client_geometries()
            end)
end



table.insert(shared.on_start_functions, schedule_reset_widgets)

capi.screen.connect_signal("list", schedule_reset_widgets)
capi.screen.connect_signal("primary_changed", schedule_reset_widgets)
capi.screen.connect_signal("list", schedule_fix_client_geometries)

awful.keyboard.append_global_keybindings{
    awful.key({ }, "XF86Launch1", function () capi.awesome.emit_signal("show_main_waffle", {anchor = "screen"}) end),
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
}

gtimer {
    timeout = 0.5,
    autostart = true,
    callback = function()
        local nscreen = capi.mouse.screen
        if nscreen ~= current_screen then
            if current_screen ~= nil then
                current_screen.widgets.indicator.context_transformation = (nil)
            end
            nscreen.widgets.indicator.context_transformation = {highlighted = true}
            -- switch active screen
            current_screen = nscreen
        end
    end
}

-- base keys and buttons
local global_keys = {
   awful.key({ "Mod1" }, "Tab",
      function ()
          yams_switcher.start{screen = awful.screen.focused()}
   end),

   awful.key({ "Mod4" }, ",", function ()
                 local c = capi.client.focus
                 if c == nil then return end
                 if c.floating then
                     return
                 end

                 local layout = c.screen.selected_tag.layout
                 if yggdrasil and layout.signature == yggdrasil.signature then
                     yggdrasil.unsplit{client = c}
                 end
             end),
   awful.key({ "Mod4" }, ".", function ()
                 local c = capi.client.focus
                 if c == nil then return end
                 if c.floating then
                     return
                 end

                 local layout = c.screen.selected_tag.layout
                 if layout.machi_set_cmd ~= nil then
                     shared.client.start_switcher(nil, false)
                 elseif yggdrasil and layout.signature == yggdrasil.signature then
                     yggdrasil.split{client = c}
                 end
             end),
   awful.key({ "Mod4" }, "Tab", function ()
                 local c = capi.client.focus
                 if c == nil then return end
                 if c.floating then
                     c.floating = false
                     return
                 end

                 local layout = c.screen.selected_tag.layout
                 if layout.machi_set_cmd ~= nil then
                     shared.client.start_switcher(c, true)
                 elseif yggdrasil and layout.signature == yggdrasil.signature then
                     yggdrasil.set_inner_layout{client = c}
                 end
             end),
   awful.key({ "Mod4" }, "/",               function () machi.default_editor.start_interactive() end),
   awful.key({ "Mod4" }, "[",               function () alayout.inc(alayout.layouts, -1) end),
   awful.key({ "Mod4" }, "]",               function () alayout.inc(alayout.layouts, 1) end),
   awful.key({ "Mod4" }, "Up",              function () go_screen_by_direction("up") end),
   awful.key({ "Mod4" }, "Left",            function () go_screen_by_direction("left") end),
   awful.key({ "Mod4" }, "Down",            function () go_screen_by_direction("down") end),
   awful.key({ "Mod4" }, "Right",           function () go_screen_by_direction("right") end),
   awful.key({ "Shift", "Mod4" }, "Up",     function () go_screen_by_direction("up", "client") end),
   awful.key({ "Shift", "Mod4" }, "Left",   function () go_screen_by_direction("left", "client") end),
   awful.key({ "Shift", "Mod4" }, "Down",   function () go_screen_by_direction("down", "client") end),
   awful.key({ "Shift", "Mod4" }, "Right",  function () go_screen_by_direction("right", "client") end),
   awful.key({ "Control", "Mod4" }, "Up",   function () go_screen_by_direction("up", "tag") end),
   awful.key({ "Control", "Mod4" }, "Left", function () go_screen_by_direction("left", "tag") end),
   awful.key({ "Control", "Mod4" }, "Down", function () go_screen_by_direction("down", "tag") end),
   awful.key({ "Control", "Mod4" }, "Right",function () go_screen_by_direction("right", "tag") end),
   awful.key({ }, "XF86AudioLowerVolume",   function () shared.action.audio_setup("volume-adjust", -5) end),
   awful.key({ }, "XF86AudioRaiseVolume",   function () shared.action.audio_setup("volume-adjust",  5) end),
   awful.key({ }, "XF86AudioMute",          function () shared.action.audio_setup("mute-toggle") end),
   awful.key({ }, "XF86MonBrightnessUp",    function () awful.spawn("adjust_brightness.sh +0.1", false) end),
   awful.key({ }, "XF86MonBrightnessDown",  function () awful.spawn("adjust_brightness.sh -0.1", false) end),
   awful.key({ "Mod4" }, "Return",          function () shared.action.terminal() end),
   awful.key({ "Mod4" }, "w",               function () shared.action.web_browser() end),
   awful.key({ "Mod4" }, "e",               function () shared.action.file_manager() end),
   awful.key({ "Mod4" }, "l",               function () shared.action.screen_locker() end),
   awful.key({ "Mod4" }, "t",               function () shared.action.calendar() end),
   awful.key({ "Mod4" }, "a",               function () capi.awesome.emit_signal("show_calendar_waffle", {anchor = "screen"}) end),
   awful.key({ "Mod4" }, "r",               function () shared.waffle.launcher() end),
   awful.key({ "Mod4" }, "p",               function () shared.waffle.zsh_completion() end),
   awful.key({ "Mod4" }, "F1",              function () shared.action.terminal_session{ name = "F1" } end),
   awful.key({ "Mod4" }, "F2",              function () shared.action.terminal_session{ name = "F2" } end),
   awful.key({ "Mod4" }, "F3",              function () shared.action.terminal_session{ name = "F3" } end),
   awful.key({ "Mod4" }, "F4",              function () shared.action.terminal_session{ name = "F4" } end),
   -- keep the both ways of showing the desktop, not sure which one is better for now.
   awful.key({ "Mod4" }, "d",               function ()
         local clients = {}
         local has_visible = false
         awful.client.focus.history.disable_tracking()
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

            for _, c in ipairs(clients) do
               c.minimized = c.orig_minimized
               c.orig_minimized = nil
            end
         end
         awful.client.focus.history.enable_tracking()
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
   awful.key({ "Mod4", "Control" }, "Escape", capi.awesome.quit),
}

-- tags and layouts

-- shared.screen.tags = { "壹", "贰", "叁", "肆" }
shared.screen.tags = {
    "☰", "☱", "☲", "☳", "☴", "☵", "☶", "☷"
}

for i = 1, #shared.screen.tags do
    local key = tostring(i)
    global_keys =
        table_join(
            {
                awful.key({ "Mod4" }, tostring(i), function ()
                              taglist.switch_or_restore(awful.screen.focused().tags[i])
                          end),
                awful.key({ "Mod4", "Control" }, tostring(i), function ()
                              local s = awful.screen.focused()
                              if #s.selected_tags == 1 then
                                  taglist.swap_tags(s.selected_tag, s.tags[i])
                              end
                          end),
                awful.key({ "Mod4", "Mod1" }, tostring(i), function ()
                              local s = awful.screen.focused()
                              awful.tag.viewtoggle(s.tags[i])
                          end),
                awful.key({ "Mod4", "Shift" }, tostring(i), function ()
                              local c = capi.client.focus
                              if c == nil then return end
                              c:toggle_tag(c.screen.tags[i])
                          end),
            },
            global_keys)
end

awful.keyboard.append_global_keybindings(global_keys)

-- initialize tags for each screen

awful.screen.connect_for_each_screen(
   function (s)
      for i, t in ipairs(shared.screen.tags) do
         local tag = awful.tag.add(t, { screen = s, layout = alayout.layouts[1], layouts = alayout.layouts })
      end

      s.tags[1]:view_only()
   end
)

local gears = require("gears")
local gsurf = require("gears.surface")
local cairo = require("lgi").cairo
function shared.screen.fetch_wallpaper(input)
    local cmd = {"wallpaper_fetcher.py", "--recolor="..beautiful.fg_normal.."-"..beautiful.bg_normal}
    if input ~= nil then
        cmd[#cmd + 1] = "-i"
        cmd[#cmd + 1] = input
    end
    awful.spawn.easy_async(cmd, function (stdout, stderr, reason, code)
            if reason ~= "exit" or code ~= 0 then
                print("Failed to fetch wallpaper. reason: "..reason..", code: "..tostring(code))
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

function shared.screen.silhouette(filename)
    local lgi = require("lgi")
    local gdebug = require("gears.debug")
    -- Safe load for optional Rsvg module
    local Rsvg = nil
    do
        local success, err = pcall(function() Rsvg = lgi.Rsvg end)
        if not success then
            gdebug.print_warning(debug.traceback("Could not load Rsvg: " .. tostring(err)))
        end
    end

    local handle, err = Rsvg.Handle.new_from_file(filename)
    if err then
        print("Error loading "..filename..": "..tostring(err))
        return
    end

    local dim = handle:get_dimensions()
    local w, h = dim.width, dim.height
    local ratio = 0.6

    for s in screen do
        local geom, cr = gears.wallpaper.prepare_context(s)
        local scale, target_w, target_h
        if geom.width / w < geom.height / h then
            scale = ratio * geom.width / w
        else
            scale = ratio * geom.height / h
        end
        if scale > 1 then scale = math.floor(scale + 0.5) end
        target_w = scale * w
        target_h = scale * h

        local stroke_width = (beautiful.xborder_width - beautiful.xborder_outer_space - beautiful.xborder_inner_space)
        local offset = beautiful.xborder_width + stroke_width
        local surf = cairo.ImageSurface(cairo.Format.ARGB32,
                                        target_w + offset + stroke_width,
                                        target_h + offset + stroke_width)
        local viewport = Rsvg.Rectangle{x = 0, y = 0, width = target_w, height = target_h}
        do
            local cr = cairo.Context(surf)

            cr:translate(stroke_width / 2, stroke_width / 2)

            cr:save()
            handle:set_stylesheet(string.format("*{stroke:none;fill:%s;}", beautiful.xborder_space));
            handle:render_document(cr, viewport)
            cr:restore()

            cr:save()
            cr:translate(offset, offset)
            cr:set_operator("ATOP")
            handle:set_stylesheet(string.format("*{stroke:none;fill:%s;}", beautiful.bg_normal));
            handle:render_document(cr, viewport)
            cr:restore()

            cr:save()
            cr:set_operator("OVER")
            local stroke_css = string.format([[
* {
  stroke:%s;
  stroke-width:%fpx;
  fill:none;
}]], beautiful.border_focus, stroke_width / scale)
            handle:set_stylesheet(stroke_css);
            handle:render_document(cr, viewport)
            cr:restore()
        end

        cr:set_operator("SOURCE")
        cr:set_source(gcolor(beautiful.bg_normal))
        cr:paint()

        cr:translate(geom.width / 2 - target_w / 2 - offset / 2, geom.height / 2 - target_h / 2 - offset / 2)

        cr:save()
        cr:translate(offset, offset)
        cr:set_operator("OVER")
        handle:set_stylesheet(string.format("*{stroke:none;fill:%s;}", beautiful.xborder_shade));
        handle:render_document(cr, viewport)
        cr:restore()

        cr:set_operator("OVER")
        cr:set_source_surface(surf)
        cr:paint()

        surf:finish()
    end
end

local launcher
awful.keyboard.append_global_keybindings{
    awful.key({ "Mod4", "Shift" }, "r", function ()
                  if launcher == nil then
                      launcher = require("bling.widget.app_launcher"){
                          prompt_icon = "🚀",
                          app_width = dpi(80),
                          app_height = dpi(80),
                          apps_per_column = 3,
                          apps_per_row = 3,
                      }
                  end
                  launcher:show()
              end),
}

return nil

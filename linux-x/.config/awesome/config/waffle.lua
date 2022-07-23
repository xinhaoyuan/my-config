local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local cwidget = require((...):match("(.-)[^%.]+$") .. "widget")
shared.waffle = {}

local capi = {
   client = client,
   screen = screen,
   awesome = awesome,
   mouse = mouse,
}

local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local waffle = require("waffle")
local cairo = require("lgi").cairo
local gfs = require("gears.filesystem")
local gsurface = require("gears.surface")
local gshape = require("gears.shape")
local gcolor = require("gears.color")
local gtimer = require("gears.timer")
local gstring = require("gears.string")
local icons = require("icons")
local fallback = require("fallback")
local fixed_margin = require("fixed_margin")
local fixed_align = require("fixed_align")
local fixed_place = require("fixed_place")
local outlined_textbox = require("outlined_textbox")
local debug_container = require("debug_container")
local masked_imagebox = require("masked_imagebox")
local acolor = require("aux").color
local mpc_gobject = require("mpc-gobject")
local orgenda = require("orgenda")
local mycalendar = require("my-calendar")
local scroller = require("scroller")
local notix = require("notix")
local fts = require("hotpot").focus_timestamp
local dpi = require("beautiful.xresources").apply_dpi
local ocontainer = require("onion.container")
local opicker = require("onion.picker")
local source = require("awemni.ng.source")
local bento = require("awemni.ng.bento")

local waffle_width = beautiful.waffle_panel_width or dpi(240)
local calendar_waffle_width = waffle_width
local button_height = beautiful.waffle_item_height or dpi(20)
local button_padding = beautiful.sep_small_size or dpi(4)
local panel_padding = beautiful.sep_big_size or dpi(10)
local font_normal = beautiful.font
local font_info = beautiful.font_minor
local update_interval_s = 2

local function em(t)
    -- return "<span color='" .. beautiful.special_normal .. "'>" .. t .. "</span>"
    return t
end

local function view(args)
    args = args or {}
    local root = args.root
    assert(root ~= nil)
    local checked = {[root] = true}
    local traverse_pool = {root}
    local view = {}
    view.keys = args.keys or {}

    -- traverse the hierachy in depth-first order
    while #traverse_pool > 0 do
        local widget = traverse_pool[#traverse_pool]
        table.remove(traverse_pool, #traverse_pool)

        for _, c in ipairs(widget:get_children()) do
            if not checked[c] then
                checked[c] = true
                table.insert(traverse_pool, c)
            end
        end

        if widget.keys then
            for k, f in pairs(widget.keys) do
                if not view.keys[k] then
                    view.keys[k] = f
                end
            end
        end
    end

    view.widget = args.no_margin and root or wibox.widget {
        root,
        margins = beautiful.useless_gap,
        widget = wibox.container.margin,
    }
    view.key_handler = args.key_handler or function (self, mods, key, event)
        if args.key_filter and not args.key_filter(self, mods, key, event) then
            return
        elseif view.keys[key] then
            view.keys[key](mods, key, event)
            return true
        elseif args.default_key_handler then
            return args.default_key_handler(self, mods, key, event)
        else
            return false
        end
    end

    view.options = {
        -- Options here.
    }

    view.on_open = args.on_open
    view.on_close = args.on_close

    return view
end

local function simple_button(args)
   local button_action = args.button_action or args.action
   local key_action = args.key_action or args.action

   local ret = wibox.widget {
      {
         args.widget,
         buttons = args.buttons,
         margins = button_padding,
         widget = wibox.container.margin,
      },
      forced_width = args.width,
      forced_height = args.height,
      fg_picker = opicker.beautiful{
          "fg_", opicker.switch{
              {"active", "focus"},
              {"hover", "focus"},
              default = "normal",
          },
      },
      bg_picker = opicker.beautiful{
          "bg_", opicker.switch{
              {"active", "focus"},
              {"hover", "focus"},
          },
      },
      margins_picker = opicker.table{
          top = opicker.branch{"active", dpi(2), 0},
          bottom = opicker.branch{"active", -dpi(2), 0},
          left = opicker.branch{"active", dpi(2), 0},
          right = opicker.branch{"active", -dpi(2), 0},
      },
      widget = ocontainer,
   }

   local function update_context_transfromation()
       ret.context_transformation = {
           hover = ret.hover,
           active = ret.active,
           highlighted = ret.hover or ret.active,
       }
   end

   ret:connect_signal(
      "mouse::enter",
      function (...)
          ret.hover = true
          for i, b in ipairs(capi.mouse.coords().buttons) do
              if b then
                  ret.active = true
                  break
              end
          end
          update_context_transfromation()
      end
   )

   ret:connect_signal(
      "mouse::leave",
      function ()
          ret.hover = nil
          ret.active = nil
          update_context_transfromation()
      end
   )

   ret:connect_signal(
      "button::press",
      function ()
          ret.active = true
          update_context_transfromation()
      end
   )

   ret:connect_signal(
      "button::release",
      function (_widget, _x, _y, button)
          if ret.active and button_action and (button == 1 or button == 3) then
              button_action(button ~= 1)
          end
          ret.active = nil
          update_context_transfromation()
      end
   )

   update_context_transfromation()

   if args.key ~= nil and key_action then
      local function cb(mod, _, event)
         for _, m in ipairs(mod) do
            mod[m] = true
         end
         if event == "press" then
            key_action(mod["Shift"])
         end
      end
      ret.keys = {}
      if type(args.key) == "table" then
         for _, k in ipairs(args.key) do
            ret.keys[k] = cb
         end
      else
         ret.keys[args.key] = cb
      end
   end

   return ret
end


local function button(args)
    local label = args.label_widget or
        wibox.widget {
                markup = args.markup,
                text = args.text,
                font = font_normal,
                forced_height = button_height,
                align = "center",
                valign = "center",
                widget = wibox.widget.textbox,
        }

    args.width = args.width or waffle_width

    args.widget = wibox.widget {
        args.icon_widget or (
            args.icon and
            wibox.widget {
                    {
                        image = args.icon,
                        resize = true,
                        forced_width = args.icon_size or button_height,
                        forced_height = args.icon_size or button_height,
                        widget = masked_imagebox,
                    },
                    halign = "center",
                    valign = "center",
                    widget = wibox.container.place,
            }
        ),
        label,
        args.indicator and {
            {
                text = args.indicator,
                font = font_normal,
                align = "center",
                valign = "center",
                widget = wibox.widget.textbox,
            },
            fg_picker = opicker.beautiful{opicker.branch{"inactive_hotkey", "minor_", "special_"}, opicker.highlighted_switcher},
            widget = ocontainer,
        },
        expand = "inside",
        layout = args.button_layout or fixed_align.horizontal,
    }

    local ret = simple_button(args)
    ret.label = label

    return ret
end

local sep_color = gcolor(acolor(beautiful.fg_normal):blend_with(acolor(beautiful.bg_normal), 0.75):to_string())
local function decorate_panel(args)
    if args.top_sep then
        return wibox.widget {
            {
                args.widget,
                draw_empty = false,
                top = panel_padding,
                widget = fixed_margin,
            },
            bgimage = function(context, cr, width, height)
                height = panel_padding
                beautiful.draw_separator(cr, width, height)
            end,
            widget = wibox.container.background
        }
    else
        return wibox.widget.base.make_widget_from_value(args.widget)
    end
end

local function decorate_waffle(widget)
    return beautiful.apply_border_to_widget {
        widget = widget,
        top = true,
        bottom = true,
        left = true,
        right = true,
    }
end

local waffle_shutdown_view = view {
    root = decorate_waffle(
        decorate_panel {
            widget = {
                button {
                    -- icon = icons.sleep,
                    markup = "Suspend",
                    indicator = em("s"),
                    key = {"s", "S"},
                    action = function (alt)
                        waffle:hide()
                        awful.spawn({"systemctl", "suspend"})
                    end
                },
                button {
                    -- icon = icons.sleep,
                    markup = "Hibernate",
                    indicator = em("h"),
                    key = {"h", "H"},
                    action = function (alt)
                        waffle:hide()
                        awful.spawn({"systemctl", "hibernate"})
                    end
                },
                button {
                    markup = "Reboot",
                    indicator = em("r"),
                    key = {"r", "R"},
                    action = function (alt)
                        waffle:hide()
                        awful.spawn({"systemctl", "reboot"})
                    end
                },
                button {
                    -- icon = icons.poweroff,
                    markup = "Power off",
                    indicator = em("p"),
                    key = {"p", "P"},
                    action = function (alt)
                        waffle:hide()
                        awful.spawn({"systemctl", "poweroff"})
                    end
                },
                layout = wibox.layout.fixed.vertical
            }
    })
}

local waffle_calendar_view
local waffle_settings_view
local waffle_tray_view
local waffle_tray_wrapper = wibox.widget {
    margins = beautiful.xborder_width,
    widget = wibox.container.margin
}
local function get_tray_item_key(index)
    if index < 10 then
        return tostring(index)
    elseif index < 36 then
        return string.char(87 + index)
    else
        return nil
    end
end
local function show_tray_view()
    -- waffle_tray_wrapper.widget = shared.screen.detach_tray_widget()
    -- waffle:show(waffle_tray_view, { mode = "push" })

    local widget = { layout = wibox.layout.fixed.vertical }
    for index, info in ipairs(awesome.systray_list()) do
        table.insert(widget, button {
                         text = info[2],
                         indicator = em(get_tray_item_key(index)),
                         key = get_tray_item_key(index),
                         action = function (alt)
                             awful.spawn({'activate-tray-window', tostring(info[1]), alt and '1' or ''}, false)
                             waffle:hide()
                         end,
        })
    end

    waffle:show(view {
                    root = decorate_waffle(
                        decorate_panel {
                            widget = wibox.widget(widget)
                        }
                    )
                     },
                { mode = "push" })
end

local battery_widget_width = waffle_width - button_height - button_padding * 3
local battery_widget
do
   local charging_color = acolor.from_string(beautiful.special_normal):blend_with(beautiful.bg_normal, 0.25):to_string()
   local background_color = beautiful.border_normal

   local battery_status_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        align = "center",
        forced_width = battery_widget_width,
        forced_height = button_height - dpi(2),
        font = font_info,
        widget = wibox.widget.textbox
    }

   local battery_percentage_widget = wibox.widget {
      max_value = 1,
      forced_width = battery_widget_width,
      forced_height = dpi(2),
      paddings = 0,
      border_width = 0,
      background_color = "#00000000",
      shape = gshape.bar,
      clip = true,
      widget = wibox.widget.progressbar
   }

   battery_widget = wibox.widget {
       {
           image = icons.battery_full,
           forced_height = button_height,
           forced_width = button_height,
           widget = wibox.widget.imagebox
       },
       {
           battery_status_widget,
           battery_percentage_widget,
           layout = wibox.layout.fixed.vertical
       },
       spacing = button_padding,
       visible = false,
       layout = wibox.layout.fixed.horizontal
   }

   -- Surface-linux
   local battery_status_command = {"mshw0084-rqst.py", "-q", "-d", "/dev/ttyS0", "bat1.pretty"}
   -- -- For debugging
   -- local battery_status_command = {"echo", "Percentage: 70%\nState:Charging\nRemaining: 10 hours"}

   local function parse_battery_output(stdout)
       local results = {}
       for line in stdout:gmatch("[^\r\n]+") do
           key, value = line:match("%s*([^:]-)%s*:%s*(.-)%s*$")
           if key == "Percentage" then
               results.value = tonumber(value:match("(%d*)%%")) / 100
           elseif key == "Remaining" then
               results.remaining = value
           elseif key == "State" then
               results.state = value
               results.charging = (value:match("Charging") ~= nil)
           end
       end
       return results
   end

   local update_graphic = function (widget, stdout)
       local status = parse_battery_output(stdout)
       battery_widget.visible = true
       battery_percentage_widget.value = status.value
       battery_percentage_widget.color = status.charging and charging_color or nil
       battery_status_widget:set_text(status.state..": "..status.remaining)
   end

   local function spawn_and_update_battery(cmd)
       awful.spawn.easy_async_with_shell(
           battery_status_command,
           function (stdout, stderr, exitreason, exitcode)
               update_graphic(battery_widget, stdout, stderr, exitreason, exitcode)
           end
       )
   end

   gtimer {
       timeout = 60,
       call_now = true,
       autostart = true,
       callback = function ()
           awful.spawn.easy_async(
               battery_status_command,
               function (stdout)
                   update_graphic(battery_widget, stdout)
               end
           )
       end,
   }
end

local music_widget
do
    -- Not used anymore
    -- local GET_STATUS_CMD = 'mpc current -f "%title% - %artist%" -q'
    -- local NEXT_CMD = 'mpc next -q'
    -- local PREV_CMD = 'mpc prev -q'
    -- local TOGGLE_CMD = 'mpc toggle -q'

    local mpd_status_widget = wibox.widget {
        text = "",
        forced_height = button_height,
        font = font_normal,
        -- outline_size = dpi(2),
        -- widget = outlined_textbox
        widget = wibox.widget.textbox
    }

    local mpd_title_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        forced_height = button_height,
        widget = wibox.widget.textbox
    }

    local mpd_meta_widget = wibox.widget {
        text = "",
        ellipsize = "end",
        font = font_info,
        forced_height = button_height,
        widget = wibox.widget.textbox
    }

    local mpd_progress_widget = wibox.widget {
        max_value     = 1,
        value         = 0,
        forced_height = dpi(2),
        background_color = "#00000000",
        widget        = wibox.widget.progressbar,
    }

    -- local lighter_fg_normal = acolor.from_string(beautiful.fg_normal):blend_with(beautiful.bg_normal, 0.75):to_string()
    -- local lighter_fg_focus = acolor.from_string(beautiful.fg_focus):blend_with(beautiful.bg_focus, 0.75):to_string()

    local mpd_icon_widget =
        wibox.widget {
            {
                image = icons.music,
                resize = true,
                forced_width = button_height,
                forced_height = button_height,
                widget = masked_imagebox,
            },
            fg_picker = opicker.beautiful{
                "fg_", opicker.highlighted_switcher},
            widget = ocontainer,
        }

    mpc_gobject:connect_signal(
        "update::status",
        function (_, status)
            if status.err then
                -- mpd_status_widget:set_text("✖")
                -- mpd_title_widget:set_text(tostring(err))
                -- mpd_meta_widget:set_text("(╯°Д°)╯ ┻━┻")
                music_widget:set_visible(false)
                return
            end

            if status.state == "play" then
                mpd_status_widget:set_text("▶")
            elseif status.state == "pause" then
                mpd_status_widget:set_text("⏸")
            elseif status.state == "stop" then
                mpd_status_widget:set_text("⏹")
            else
                mpd_status_widget:set_text(status.state)
            end

            mpd_progress_widget:set_value(status.progress or 0)
            music_widget:set_visible(true)
        end
    )

    mpc_gobject:connect_signal(
        "update::song",
        function (_, song)
            mpd_title_widget:set_text(song.title or "")

            local meta = {}
            if song.artist then
                meta[#meta + 1] = song.artist
            end
            if song.album then
                meta[#meta + 1] = song.album
            end

            mpd_meta_widget:set_text(table.concat(meta, " - "))
        end
    )

    music_widget = button {
        icon_widget = wibox.widget {
            {
                mpd_status_widget,
                widget = wibox.container.place
            },
            {
                mpd_icon_widget,
                widget = wibox.container.place
            },
            layout = wibox.layout.fixed.vertical,
        },
        label_widget = wibox.widget {
            {
                {
                    {
                        {
                            mpd_title_widget,
                            speed = 100,
                            step_function = wibox.container.scroll.step_functions
                                .waiting_nonlinear_back_and_forth,
                            layout = wibox.container.scroll.horizontal,
                        },
                        widget = wibox.container.place
                    },
                    mpd_progress_widget,
                    {
                        {
                            {
                                mpd_meta_widget,
                                speed = 100,
                                step_function = wibox.container.scroll.step_functions
                                    .waiting_nonlinear_back_and_forth,
                                layout = wibox.container.scroll.horizontal,
                            },
                            widget = wibox.container.place
                        },
                        {
                            text = "_(:3」∠)_",
                            align = "center",
                            force_height = button_height - dpi(2),
                            font = font_info,
                            widget = wibox.widget.textbox
                        },
                        widget = fallback
                    },
                    layout = wibox.layout.fixed.vertical,
                },
                draw_empty = false,
                left = button_padding,
                right = button_padding,
                widget = fixed_margin,
            },
            {
                text = "Music",
                align = "center",
                valign = "center",
                forced_height = button_height,
                widget = wibox.widget.textbox,
            },
            widget = fallback,
        },
        indicator = em("m"),
        key = {"m", "M"},
        action = function (alt)
            waffle:hide()
            shared.action.music_app()
        end,
        buttons = awful.util.table.join(
            awful.button({ }, 1, function () waffle:hide(); shared.action.music_app() end),
            awful.button({ }, 3, function () mpc_gobject:toggle_play() end),
            awful.button({ }, 4, function ()
                    mpc_gobject:go_next()
            end),
            awful.button({ }, 5, function ()
                    mpc_gobject:go_previous()
            end)
        ),
    }

    music_widget.keys["Left"] = function (mod, _key, event)
        if event == "press" then return end
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:go_previous()
        end
    end
    music_widget.keys["Right"] = function (mod, _key, event)
        if event == "press" then return end
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:go_next()
        end
    end
    music_widget.keys["Up"] = function (mod, _key, event)
        if event == "press" then return end
        for _, m in ipairs(mod) do mod[m] = true end
        if mod["Shift"] then
            mpc_gobject:toggle_play()
        end
    end
end

local waffle_root_view
local waffle_root_source_mode
local waffle_dashboard_view
local waffle_dashboard_status_widget = decorate_panel{
    widget = {
        {
            button {
                label_widget = {
                    {
                        {
                            image = icons.cpu,
                            forced_height = button_height,
                            forced_width = button_height,
                            widget = masked_imagebox,
                        },
                        {
                            cwidget.cpu_widget,
                            forced_height = button_height,
                            left = button_padding,
                            right = button_padding,
                            widget = wibox.container.margin
                        },
                        layout = wibox.layout.align.horizontal
                    },
                    {
                        {
                            image = icons.ram,
                            forced_height = button_height,
                            forced_width = button_height,
                            widget = masked_imagebox,
                        },
                        {
                            cwidget.ram_widget,
                            forced_height = button_height,
                            left = button_padding,
                            right = button_padding,
                            widget = wibox.container.margin
                        },
                        expand = "inside",
                        layout = wibox.layout.align.horizontal
                    },
                    spacing = button_padding * 2,
                    layout = wibox.layout.fixed.vertical,
                },
                key = {"x", "X"},
                indicator = em("x"),
                action = function (alt)
                    shared.action.terminal({"htop"})
                    waffle:hide()
                end,
            },
            button {
                label_widget = wibox.widget{
                    cwidget.net_widget,
                    forced_height = button_height,
                    widget = wibox.container.background,
                },
                indicator = em("n"),
                key = {"n", "N"},
                action = function (alt)
                    if alt then
                        -- TODO make this an action.
                        awful.spawn({"expressvpn", cwidget.net_widget.has_vpn and "disconnect" or "connect"}, false)
                    else
                        shared.action.terminal({"sudo", "iftop"})
                    end
                    waffle:hide()
                end,
            },
            button {
                label_widget = wibox.widget{
                    cwidget.disk_widget,
                    forced_height = button_height,
                    widget = wibox.container.background,
                },
                indicator = em("d"),
                key = {"d", "D"},
                action = function (alt)
                    shared.action.terminal({"sudo", "iotop"})
                    waffle:hide()
                end,
            },
            {
                battery_widget,
                margins = button_padding,
                draw_empty = false,
                widget = fixed_margin,
            },
            layout = wibox.layout.fixed.vertical,
        },
        widget = wibox.container.margin,
    }
}

local function icon_label(icon)
    return wibox.widget{
        {
            image = icon,
            resize = true,
            widget = masked_imagebox,
        },
        halign = "center",
        valign = "center",
        widget = wibox.container.place,
    }
end

local grid_height = dpi(48)
local grid_width = waffle_width / 3
local grid_button_layout = fixed_align.vertical
local waffle_dashboard_action_grid_widget = decorate_panel {
    top_sep = true,
    widget = {
        {
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.launcher),
                indicator = em("r"),
                key = {"r", "R"},
                action = function (alt)
                    if waffle:is_in_view(waffle_root_view) then
                        waffle_root_source_mode = nil
                        waffle_root_view:reload_source()
                        waffle_dashboard_view.active = false
                    end
                end,
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.terminal),
                markup = "Terminal",
                indicator = em("↵"),
                key = "Return",
                action = function (alt)
                    if alt then
                        awful.spawn("xterm")
                    else
                        shared.action.terminal()
                    end
                    waffle:hide()
                end
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.browser),
                markup = "Web",
                indicator = em("w"),
                key = {"w", "W"},
                action = function (alt)
                    shared.action.web_browser()
                    waffle:hide()
                end
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.file_manager),
                markup = "Files",
                indicator = em("e"),
                key = {"e", "E"},
                action = function (alt)
                    shared.action.file_manager()
                    waffle:hide()
                end
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.dots),
                markup = "Tray",
                indicator = em("y"),
                key = {"y", "Y"},
                action = function (alt)
                    show_tray_view()
                end,
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.fortune),
                markup = "Fortune",
                indicator = em("f"),
                key = {"f", "F"},
                action = function (alt)
                    if alt then
                        shared.screen.xkcd()
                    else
                        local fortune = shared.screen.get_fortune()
                        if fortune then
                            shared.action.web_browser("? " .. fortune)
                        end
                    end
                    waffle:hide()
                end
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.setup),
                markup = "Settings",
                indicator = em("s"),
                key = {"s", "S"},
                action = function (alt)
                    waffle:show(waffle_settings_view, { mode = "push" })
                end,
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.lock),
                markup = "Lock",
                indicator = em("l"),
                key = {"l", "L"},
                action = function (alt)
                    shared.action.screen_locker()
                    waffle:hide()
                end
            },
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.poweroff),
                markup = "Power",
                indicator = em("u"),
                key = {"u", "U"},
                action = function (alt)
                    waffle:show(waffle_shutdown_view, { mode = "push" })
                end
            },
            homogeneous   = true,
            forced_num_cols = 3,
            layout = wibox.layout.grid,
        },
        widget = wibox.container.place,
    },
}

local waffle_dashboard_action_list_widget = decorate_panel {
    top_sep = true,
    widget = {
        button {
            icon = icons.audio,
            label_widget = wibox.widget{
                {
                    cwidget.audio_sink_widget,
                    width = waffle_width - beautiful.icon_size * 2 - button_padding * 2,
                    height = button_height * 2,
                    strategy = "max",
                    widget = wibox.container.constraint,
                },
                widget = wibox.container.place
            },
            indicator = em("p"),
            key = {"p", "P"},
            action = function (alt)
                -- if cwidget.audio_sink_widget:execute(alt) then
                --     waffle:hide()
                -- end
                if not alt and waffle:is_in_view(waffle_root_view) then
                    waffle_root_source_mode = "audio_sink"
                    waffle_root_view:reload_source()
                    waffle_dashboard_view.active = false
                end
            end,
        },
        button {
            icon = icons.mic,
            label_widget = wibox.widget{
                {
                    cwidget.audio_source_widget,
                    width = waffle_width - beautiful.icon_size * 2 - button_padding * 2,
                    height = button_height * 2,
                    strategy = "max",
                    widget = wibox.container.constraint,
                },
                widget = wibox.container.place
            },
            indicator = em("i"),
            key = {"i", "I"},
            action = function (alt)
                -- if cwidget.audio_source_widget:execute(alt) then
                --     waffle:hide()
                -- end
                if not alt and waffle:is_in_view(waffle_root_view) then
                    waffle_root_source_mode = "audio_source"
                    waffle_root_view:reload_source()
                    waffle_dashboard_view.active = false
                end
            end,
        },
        button {
            icon = icons.calendar_todo,
            label_widget = wibox.widget {
                align = "center",
                format = "%Y-%m-%d %a %H:%M",
                widget = wibox.widget.textclock,
            },
            indicator = em("a"),
            key = {"a", "A"},
            action = function (alt)
                if alt then
                    shared.action.calendar()
                    waffle:hide()
                else
                    waffle:show(waffle_calendar_view, { mode = "push" })
                end
            end,
        },
        music_widget,
        layout = wibox.layout.fixed.vertical,
    },
}

waffle_dashboard_view = view {
    root = wibox.widget{
        {
            waffle_dashboard_status_widget,
            waffle_dashboard_action_grid_widget,
            waffle_dashboard_action_list_widget,
            layout = wibox.layout.fixed.vertical,
        },
        context_transformation = {
            inactive_hotkey = true,
        },
        widget = ocontainer,
    },
    no_margin = true,
    key_filter = function (_self, mod, key, event)
        if event ~= "press" then return true end
        if key:find("^XF86Launch.*") then
            waffle:hide()
            return
        end
        if (key == "Left" or key == "Right" or key == "Up" or key == "Down") then
            pcall(awful.screen.focus_bydirection, key:lower())
            waffle:hide()
            capi.awesome.emit_signal("show_main_waffle", {anchor = "screen"})
            return
        end
        return true
    end,
}

local lgi = require("lgi")
local gio = lgi.Gio
local gtk = lgi.require("Gtk", "3.0")
local gtk_icon_theme = gtk.IconTheme.get_default()
-- Force build the cache.
gtk_icon_theme:load_icon(gtk_icon_theme:get_example_icon_name(), beautiful.icon_size, 0)

local cached_execution_path = gfs.get_cache_dir() .. "/history_app_execution"
local cached_execution = {}
function restore_cache_execution()
    cached_execution = {}
    local f, err = io.open(cached_execution_path, "r")
    if err then return end
    for line in f:lines() do
        table.insert(cached_execution, line)
        cached_execution[line] = #cached_execution
    end
    f:close()
end
restore_cache_execution()

function cache_exeuction(name)
    local last = nil
    for i = 1, #cached_execution + 1  do
        last, cached_execution[i] = cached_execution[i], last
        if i > 1 then cached_execution[cached_execution[i]] = i end
        if last == name then break end
    end
    cached_execution[1] = name
    cached_execution[name] = 1
end

function save_cache_execution()
    local f, err = io.open(cached_execution_path, "w")
    if err then
        return
    end
    for _, r in ipairs(cached_execution) do
        f:write(r.."\n")
    end
    f:close()
end
capi.awesome.connect_signal("exit", save_cache_execution)

function get_apps_widget_source()
    local apps = {}
    for _, ai in ipairs(gio.AppInfo.get_all()) do
        if ai:should_show() then
            apps[#apps + 1] = {
                ai = ai,
                name = ai:get_name(),
                display_name = ai:get_display_name(),
            }
        end
    end
    table.sort(apps, function (a, b)
                   local ac = cached_execution[a.name]
                   local bc = cached_execution[b.name]
                   if ac and bc then return ac < bc end
                   if ac or bc then return bc == nil end
                   return a.name < b.name
               end)
    local apps_source = source.filterable{
        upstream = source.array_proxy{
            array = apps,
        },
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.display_name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            order = function (a, b) if a == b then return nil end return a < b end,
            post_filter = function (e)
                local icon_path
                repeat
                    local icon = e.ai:get_icon()
                    if icon == nil then break end
                    local icon_info = gtk_icon_theme:lookup_by_gicon(icon, beautiful.icon_size, 0)
                    if icon_info == nil then break end
                    icon_path = icon_info:get_filename()
                until true
                local w = wibox.widget{
                    {
                        {
                            {
                                {
                                    image = icon_path,
                                    forced_width = beautiful.icon_size,
                                    forced_height = beautiful.icon_size,
                                    widget = wibox.widget.imagebox,
                                },
                                right = beautiful.sep_small_size,
                                widget = wibox.container.margin,
                            },
                            {
                                {
                                    {
                                        text = e.display_name,
                                        widget = wibox.widget.textbox,
                                    },
                                    {
                                        id = "desc",
                                        {
                                            {
                                                font = font_info,
                                                text = e.ai:get_description(),
                                                widget = wibox.widget.textbox,
                                            },
                                            height = 60,
                                            strategy = "max",
                                            widget = wibox.container.constraint,
                                        },
                                        top = dpi(2),
                                        left = dpi(2),
                                        right = dpi(2),
                                        draw_empty = false,
                                        visible = false,
                                        widget = wibox.container.margin,
                                    },
                                    layout = wibox.layout.fixed.vertical,
                                },
                                valign = "center",
                                halign = "left",
                                widget = wibox.container.place,
                            },
                            layout = fixed_align.horizontal,
                        },
                        margins = dpi(2),
                        widget = wibox.container.margin,
                    },
                    fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
                    bg_picker = opicker.beautiful{"bg_", opicker.branch{"highlighted", "focus"}},
                    widget = ocontainer,
                }
                function w:execute()
                    cache_exeuction(e.name)
                    awful.spawn(e.ai:get_executable())
                    waffle:hide()
                end
                function w:set_focused(v)
                    w:get_children_by_id("desc")[1].visible = v
                    self.context_transformation = {highlighted = v}
                end
                return w
            end,
        },
    }
    return apps_source
end

function get_audio_sink_switch_source()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            order = function (a, b) if a == b then return nil end return a < b end,
            post_filter = function (e)
                local w = wibox.widget{
                    {
                        {
                            text = e.name,
                            widget = wibox.widget.textbox,
                        },
                        margins = dpi(2),
                        widget = wibox.container.margin,
                    },
                    fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
                    bg_picker = opicker.beautiful{"bg_", opicker.branch{"highlighted", "focus"}},
                    widget = ocontainer,
                }
                function w:execute()
                    awful.spawn({"pactl", "set-default-sink", tostring(e.id)})
                    waffle:hide()
                end
                function w:set_focused(v)
                    self.context_transformation = {highlighted = v}
                end
                return w
            end,
        },
    }
    awful.spawn.easy_async_with_shell(
        [[pacmd list-sinks | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/device.description = "([^"]*)"/,m){print id"="m[1]}']],
        function (stdout, _stderr, _exitreason, _exitcode)
            for id, name in string.gmatch(stdout, "([^\n\r]*)=([^\n\r]*)") do
                output:append{id = id, name = name}
            end
        end
    )
    return ret
end

function get_audio_source_switch_source()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if f == nil then return 0 end
                f = f:lower()
                local n = e.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            order = function (a, b) if a == b then return nil end return a < b end,
            post_filter = function (e)
                local w = wibox.widget{
                    {
                        {
                            text = e.name,
                            widget = wibox.widget.textbox,
                        },
                        margins = dpi(2),
                        widget = wibox.container.margin,
                    },
                    fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
                    bg_picker = opicker.beautiful{"bg_", opicker.branch{"highlighted", "focus"}},
                    widget = ocontainer,
                }
                function w:execute()
                    awful.spawn({"pactl", "set-default-source", tostring(e.id)})
                    waffle:hide()
                end
                function w:set_focused(v)
                    self.context_transformation = {highlighted = v}
                end
                return w
            end,
        },
    }
    awful.spawn.easy_async_with_shell(
        [[pacmd list-sources | awk 'match($0,/index:\s*([0-9]*)/,m){id=m[1]} match($0,/device.description = "([^"]*)"/,m){print id"="m[1]}']],
        function (stdout, _stderr, _exitreason, _exitcode)
            for id, name in string.gmatch(stdout, "([^\n\r]*)=([^\n\r]*)") do
                output:append{id = id, name = name}
            end
        end
    )
    return ret
end

local function get_autorandr_config(contents)
    local outputs = {}
    local current_output_name
    for line in contents:gmatch("[^\n\r]+") do
        repeat
            local output = line:match("^output%s+(.-)$")
            if output then
                current_output_name = output
                break
            end
            local mode_w, mode_h = line:match("^mode%s+([0-9]+)x([0-9]+)$")
            if current_output_name and mode_w and mode_h then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].width = mode_w
                outputs[current_output_name].height = mode_h
                break
            end
            local pos_x, pos_y = line:match("^pos%s+([0-9]+)x([0-9]+)$")
            if current_output_name and pos_x and pos_y then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].x = pos_x
                outputs[current_output_name].y = pos_y
                break
            end
            local rotate = line:match("^rotate%s+(.-)$")
            if current_output_name and rotate then
                if outputs[current_output_name] == nil then
                    outputs[current_output_name] = {}
                end
                outputs[current_output_name].rotate = rotate
                break
            end
        until true
    end
    local overall_width, overall_height
    for k, v in pairs(outputs) do
        if v.rotate == "left" or v.rotate == "right" then
            v.width, v.height = v.height, v.width
        end
        if overall_width == nil or v.x + v.width > overall_width then
            overall_width = v.x + v.width
        end
        if overall_height == nil or v.y + v.height > overall_height then
            overall_height = v.y + v.height
        end
    end
    return {
        overall_width = overall_width,
        overall_height = overall_height,
        outputs = outputs,
    }
end

local rotate_label = {
    normal = "",
    left = "←",
    right = "→",
    inverted = "↓",
}
local function draw_autorandr_config(config, cr, width, height)
    local scale
    if width / config.overall_width < height / config.overall_height then
        scale = width / config.overall_width
        cr:translate(0, (height - config.overall_height * scale) / 2)
    else
        scale = height / config.overall_height
        cr:translate((width - config.overall_width * scale) / 2, 0)
    end
    local pl = lgi.Pango.Layout.create(cr)
    for k, v in pairs(config.outputs) do
        cr:set_source(gcolor(beautiful.fg_normal))
        cr:rectangle(v.x * scale, v.y * scale, v.width * scale, v.height * scale)
        cr:fill()
        local lw, lh
        local current_font_size = dpi(beautiful.font_size_normal)
        local label = k
        if v.rotate then
            label = label.."\n"..rotate_label[v.rotate]
        end
        for i = 1, 2 do
            pl:set_font_description(beautiful.get_merged_font(beautiful.font, current_font_size))
            pl:set_text(label)
            pl:set_alignment("CENTER")
            lw, lh = pl:get_size()
            lw = lw / lgi.Pango.SCALE
            lh = lh / lgi.Pango.SCALE
            if lw <= v.width * scale and lh <= v.height * scale then break end
            current_font_size = current_font_size * math.min(v.width * scale / lw, v.height * scale / lh)
        end
        cr:move_to(v.x * scale + (v.width * scale - lw) / 2, v.y * scale + (v.height * scale - lh) / 2)
        cr:set_source(gcolor(beautiful.fg_focus))
        cr:show_layout(pl)
    end
end

local screen_layout_preview_max_size = dpi(120)
local function get_screen_layout_source()
    local output = source.array_proxy()
    local ret = source.filterable{
        upstream = output,
        callbacks = {
            filter = function (f, e)
                if e == nil then return false end
                if f == nil then return 0 end
                f = f:lower()
                local n = e.info.name:lower()
                local succ, start = pcall(string.find, n, f)
                return succ and start
            end,
            order = function (a, b) if a == b then return nil end return a < b end,
            post_filter = function (e)
                local w = wibox.widget{
                    {
                        {
                            e.config and {
                                forced_width = screen_layout_preview_max_size,
                                forced_height = math.min(
                                    screen_layout_preview_max_size,
                                    math.ceil(e.config.overall_height / e.config.overall_width *
                                              screen_layout_preview_max_size)),
                                bgimage = function (context, cr, width, height)
                                    draw_autorandr_config(e.config, cr, width, height)
                                end,
                                widget = wibox.container.background,
                            },
                            {
                                {
                                    text = e.info.name..
                                        (e.info.detected and " (detected)" or "")..
                                        (e.info.current and " (current)" or ""),
                                    widget = wibox.widget.textbox,
                                },
                                left = dpi(2),
                                widget = wibox.container.margin,
                            },
                            layout = fixed_align.horizontal,
                        },
                        margins = dpi(2),
                        widget = wibox.container.margin,
                    },
                    fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
                    bg_picker = opicker.beautiful{"bg_", opicker.branch{"highlighted", "focus"}},
                    widget = ocontainer,
                }
                function w:execute()
                    local cmd
                    cmd = {"autorandr", e.info.name}
                    waffle:hide()
                    awful.spawn(cmd, false)
                end
                function w:set_focused(v)
                    -- local preview = self:get_children_by_id("preview")[1]
                    if preview then preview.visible = v end
                    self.context_transformation = {highlighted = v}
                end
                return w
            end,
        },
    }
    local home_dir = os.getenv("HOME")
    awful.spawn.easy_async(
        "autorandr",
        function (stdout, _stderr, _exitreason, _exitcode)
            local configs = {}
            for line in string.gmatch(stdout, "[^\n\r]+") do
                local name, attr = line:match("^([^%s]+)%s*(.-)$")
                table.insert(configs, {name = name, detected = attr:find("(detected)") ~= nil, current = attr:find("(current)") ~= nil})
            end
            table.sort(
                configs, function (a, b)
                    if a.detected ~= b.detected then return a.detected end
                    return a.name < b.name
                end)
            local index = 0
            for _, info in ipairs(configs) do
                index = index + 1
                local this_index = index
                local this_info = info
                local file = gio.File.new_for_path(home_dir.."/.config/autorandr/"..info.name.."/config")
                file:load_contents_async(
                    nil,
                    function (_source, result)
                        local contents, _error = file:load_contents_finish(result)
                        local config
                        if contents then
                            config = get_autorandr_config(contents)
                        end
                        output.array[this_index] = {info = this_info, config = config}
                        output:emit_signal("property::children")
                    end)
            end
            output:set_size(index)
        end
    )
    return ret
end

function get_source()
    local indicator = waffle_root_view.widget:get_children_by_id("source_indicator")[1]
    if waffle_root_source_mode == "audio_sink" then
        indicator.image = icons.audio
        return get_audio_sink_switch_source()
    elseif waffle_root_source_mode == "audio_source" then
        indicator.image = icons.mic
        return get_audio_source_switch_source()
    elseif waffle_root_source_mode == "screen_layout" then
        indicator.image = icons.monitor
        return get_screen_layout_source()
    end
    indicator.image = icons.launcher
    return get_apps_widget_source()
end

waffle_dashboard_view._private = {}
setmetatable(waffle_dashboard_view,
             { __index = function (self, index)
                   return self._private[index]
               end,
               __newindex = function (self, index, value)
                   if self._private[index] == value then return end
                   if index == "active" then
                       self.widget.visible = value
                   elseif index == "direct_hotkey" then
                       self.widget.context_transformation = {
                           inactive_hotkey = not value
                       }
                   end
                   self._private[index] = value
               end,
             })
local dashboard_key_handler = waffle_dashboard_view.key_handler
waffle_dashboard_view.key_handler = function (self, mods, key, event)
    if key == "Super_L" or key == "Super_R" or key == "Alt_L" or key == "Alt_R" then
        self.direct_hotkey = event == "press"
        return true
    elseif key == " " then
        if event == "press" then
            self.direct_hotkey = not self.direct_hotkey
        end
        return true
    end
    local pass = #key > 1 or self.direct_hotkey
    return pass and dashboard_key_handler(self, mods, key, event)
end
waffle_dashboard_view.on_open = function (self)
    waffle_root_view.widget:get_children_by_id("input_area")[1].visible = not waffle:autohide()
end
waffle_dashboard_view.on_close = function (self)
    waffle_root_source_mode = nil
    self.direct_hotkey = false
end

waffle_root_view = bento{
    container = wibox.widget{
        beautiful.apply_border_to_widget_template{
            widget = {
                waffle_dashboard_view.widget,
                {
                    {
                        {
                            {
                                {
                                    {
                                        {
                                            {
                                                {
                                                    id = "source_indicator",
                                                    widget = masked_imagebox,
                                                },
                                                height = button_height,
                                                width = button_height,
                                                strategy = "exact",
                                                widget = wibox.container.constraint,
                                            },
                                            right = beautiful.sep_small_size,
                                            widget = wibox.container.margin,
                                        },
                                        {
                                            id = "input_area",
                                            {
                                                {
                                                    {
                                                        id = "input_widget",
                                                        align = "center",
                                                        widget = wibox.widget.textbox,
                                                    },
                                                    fill_horizontal = true,
                                                    widget = wibox.container.place,
                                                },
                                                margins = dpi(2),
                                                widget = wibox.container.margin,
                                            },
                                            bg_picker = opicker.concat{opicker.beautiful{"fg_normal"}, "20"},
                                            widget = ocontainer,
                                        },
                                        layout = fixed_align.horizontal,
                                    },
                                    widget = wibox.container.place,
                                },
                                {
                                    {
                                        id = "list_container",
                                        widget = wibox.container.background,
                                    },
                                    top = dpi(4),
                                    widget = wibox.container.margin,
                                },
                                layout = fixed_align.vertical
                            },
                            margins = dpi(4),
                            widget = wibox.container.margin,
                        },
                        fg_picker = opicker.beautiful{"fg_normal"},
                        widget = ocontainer,
                    },
                    width = dpi(500),
                    height = dpi(400),
                    strategy = "exact",
                    widget = wibox.container.constraint,
                },
                layout = fallback,
            },
            top = true,
            bottom = true,
            left = true,
            right = true,
        },
        margins = beautiful.useless_gap,
        widget = wibox.container.margin,
    },
    cover = waffle_dashboard_view,
    source_generator = get_source,
}
function waffle_root_view:handle_back()
    if not waffle_dashboard_view.active then
        -- Go back to dashboard.
        self:key_handler({}, "Escape", "press")
        self:key_handler({}, "BackSpace", "press")
        return true
    end
end
function shared.waffle.launcher()
    waffle_dashboard_view.active = false
    waffle_root_source_mode = nil
    waffle:show(nil, {anchor = "screen"})
end

waffle:set_root_view(waffle_root_view)

capi.awesome.connect_signal(
    "show_main_waffle",
    function (args)
        waffle:show(nil, args)
    end
)

capi.awesome.connect_signal(
    "show_calendar_waffle",
    function (args)
        waffle:show(waffle_calendar_view, args)
    end
)

capi.awesome.connect_signal(
    "toggle_calendar_waffle",
    function (args)
        if waffle:is_in_view(waffle_calendar_view) and not waffle:autohide() then
            waffle:hide()
        else
            waffle:show(waffle_calendar_view, args)
        end
    end
)

-- Calendar

local today = os.date("*t")
local last_mid_update_timestamp = nil
local active_dates = {}
local cal_widget = wibox.widget {
    date = os.date('*t'),
    font = beautiful.font_mono,
    week_numbers = true,
    start_sunday = true,
    long_weekdays = true,
    spacing = 0,
    fn_embed = function (widget, flag, date)
        if flag == "header" then
            local sign
            if date.year == today.year then
                if date.month == today.month then
                    sign = 0
                else
                    sign = date.month < today.month and -1 or 1
                end
            else
                sign = date.year < today.year and -1 or 1
            end
            widget.font = beautiful.font_name_normal..' 12'
            widget = wibox.widget{
                sign > 0 and {
                    text = " <<",
                    align = "left",
                    font = widget.font,
                    widget = wibox.widget.textbox,
                },
                widget,
                sign < 0 and {
                    text =  ">> ",
                    align = "right",
                    font = widget.font,
                    widget = wibox.widget.textbox,
                },
                expand = "outside",
                layout = wibox.layout.align.horizontal,
            }
            return widget
        elseif flag == "month" then
            return widget
        elseif flag == "weekday" or flag == "weeknumber" then
            widget.font = font_info
            widget = wibox.widget{
                widget,
                fg_picker = opicker.beautiful{"minor_", opicker.highlighted_switcher},
                widget = ocontainer,
            }
        end

        local inverted = false

        if flag == "normal" or flag == "focus" then
            if today.year == date.year and today.month == date.month and today.day == date.day then
                inverted = true
            end

            if active_dates[date.year] and active_dates[date.year][date.month] and active_dates[date.year][date.month][date.day] then
                widget = wibox.widget {
                    widget,
                    fg_picker = opicker.beautiful{
                        "special_", opicker.branch{"inverted", "focus", "normal"}},
                    widget = ocontainer,
                }
            end
        end
        widget = wibox.widget{
            widget,
            halign = "center",
            widget = wibox.container.place,
        }

        if inverted then
            return wibox.widget {
                {
                    widget,
                    margins = dpi(2),
                    widget = wibox.container.margin
                },
                shape = function (cr, width, height)
                    if beautiful.xborder_radius and beautiful.xborder_radius >= beautiful.xborder_width then
                        beautiful.rect_with_corners(cr, width, height, true, true, true, true,
                                                    beautiful.xborder_radius - beautiful.xborder_width)
                    else
                        beautiful.rect_with_corners(cr, width, height)
                    end
                end,
                fg_picker = opicker.beautiful{"fg_focus"},
                bg_picker = opicker.beautiful{"bg_focus"},
                context_transformation = {inverted = true},
                widget = ocontainer,
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

local function cal_switch(delta)
    local date = cal_widget:get_date()
    if delta.day ~= nil then date.day = date.day + delta.day end
    if delta.month ~= nil then date.month = date.month + delta.month end
    if delta.year ~= nil then date.year = date.year + delta.year end
    cal_widget:set_date(nil)
    cal_widget:set_date(date)
end

local function cal_reset()
    cal_widget:set_date(nil)
    cal_widget:set_date(os.date('*t'))
end

local function cal_refresh()
    local date = cal_widget:get_date()
    cal_widget:set_date(nil)
    cal_widget:set_date(os.date('*t'))
end

cal_widget:connect_signal(
    "button::press",
    function (_, x, y, button)
        if button == 2 then
            cal_reset()
        elseif button == 1 or button == 4 then
            cal_switch{month = -1}
        elseif button == 3 or button == 5 then
            cal_switch{month = 1}
        end
    end)

gtimer {
    timeout = 10,
    autostart = true,
    call_now = true,
    callback = function()
        local new_today = os.date("*t")
        if today.day ~= new_today.day then
            today = new_today
            cal_refresh()
        end
    end
}

local orgenda_header
do
    orgenda_header = wibox.widget{
        {
            nil,
            {
                markup = "<span size='large'>TODO</span>",
                widget = wibox.widget.textbox,
            },
            {
                {
                    {
                        image = icons.refresh,
                        resize = true,
                        forced_height = button_height,
                        forced_width = button_height,
                        widget = masked_imagebox,
                    },
                    margins = 0,
                    widget = wibox.container.margin,
                },
                halign = "right",
                widget = wibox.container.place,
            },
            expand = "outside",
            layout = wibox.layout.align.horizontal,
        },
        fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
        bg_picker = opicker.beautiful{"bg_", opicker.highlighted_switcher},
        context_transformation = {highlighted = false},
        widget = ocontainer,
    }
    orgenda_header:connect_signal(
        "mouse::enter",
        function ()
            orgenda_header.context_transformation = {highlighted = true}
        end
    )
    orgenda_header:connect_signal(
        "mouse::leave",
        function ()
            orgenda_header.context_transformation = {highlighted = false}
        end
    )
    orgenda_header:connect_signal(
        "button::release",
        function ()
            orgenda.schedule_reset()
        end
    )
end

orgenda.data:connect_signal(
    "property::items",
    function ()
        active_dates = {}
        for _, item in ipairs(orgenda.data.items) do
            if item.timestamp and not item.done then
                local date = os.date("*t", item.timestamp)
                local y = active_dates[date.year] or {}
                local m = y[date.month] or {}
                m[date.day] = true
                y[date.month] = m
                active_dates[date.year] = y
            end
        end
        cal_refresh()
    end
)

local organda_color_func = function (priority, done)
    local key = "orgenda_color_p"..tostring(priority)..(done and "_done" or "_todo")
    return beautiful[key]
end
function orgenda_get_icon(item)
    return beautiful["orgenda_icon_p"..tostring(item.priority).."_"..(item.done and "done" or "todo")]
end
local orgenda_items_widget = orgenda.widget{
    create_item_widget_cb = function ()
        local widget = wibox.widget{
            {
                {
                    {
                        {
                            id = "icon_role",
                            {
                                forced_width = beautiful.icon_size,
                                forced_height = beautiful.icon_size,
                                widget = masked_imagebox,
                            },
                            widget = ocontainer,
                        },
                        valign = "top",
                        widget = wibox.container.place
                    },
                    right = beautiful.sep_small_size,
                    widget = wibox.container.margin,
                },
                {
                    {
                        {
                            id = "timestamp_role",
                            widget = wibox.widget.textbox
                        },
                        {
                            {
                                id = "text_role",
                                ellipsize = "none",
                                align = "left",
                                valign = "center",
                                wrap = "word_char",
                                widget = wibox.widget.textbox
                            },
                            fill_horizontal = true,
                            content_fill_horizontal = true,
                            widget = wibox.container.place,
                        },
                        layout = wibox.layout.fixed.vertical
                    },
                    valign = "center",
                    widget = wibox.container.place,
                },
                layout = wibox.layout.fixed.horizontal
            },
            fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
            bg_picker = opicker.beautiful{"bg_", opicker.branch{"highlighted", "focus"}},
            context_transformation = {highlighted = false},
            widget = ocontainer,
        }
        function widget:set_focused(f)
            self._private.focused = f
            self.context_transformation = {highlighted = f}
        end
        function widget:execute()
            if self._private.focused then
                orgenda.toggle_done(self.item)
            end
        end
        widget:connect_signal(
            "button::release",
            function (self, _x, _y, button)
                if button == 2 then
                    orgenda.hide(self.item)
                elseif button == 3 then
                    orgenda.promote(self.item)
                end
            end
        )
        return widget
    end,
    update_item_widget_cb = function (widget, item)
        widget:get_children_by_id("icon_role")[1].widget.image = orgenda_get_icon(item)
        widget:get_children_by_id("icon_role")[1].fg_picker = opicker.wrap{
            organda_color_func,
            item.priority,
            item.done,
        }
        widget:get_children_by_id("text_role")[1].markup = item.decorated_text
        widget.item = item
    end,
}
local orgenda_widget = wibox.widget{
    {
        orgenda_header,
        {
            {
                orgenda_items_widget,
                widget = scroller,
            },
            {
                {
                    markup = "<span size='large' foreground='"..beautiful.minor_normal.."'>Wow!\nNo TODOs!\nSo clean!</span>",
                    align = "center",
                    widget = wibox.widget.textbox,
                },
                widget = wibox.container.place,
            },
            widget = fallback,
        },
        layout = wibox.layout.align.vertical,
    },
    fill_vertical = true,
    content_fill_vertical = true,
    valign = "top",
    widget = fixed_place,
}

orgenda_widget.visible = shared.vars.show_notes
shared.vars:connect_signal(
    "property::show_notes",
    function(_, value)
        orgenda_widget.visible = value
    end
)

-- waffle_calendar_view = view {
--     root = decorate_waffle{
--         {
--             {
--                 {
--                     cal_widget,
--                     {
--                         {
--                             orgenda_widget,
--                             top = beautiful.sep_big_size,
--                             draw_empty = false,
--                             widget = fixed_margin,
--                         },
--                         bgimage = function(context, cr, width, height)
--                             height = beautiful.sep_big_size
--                             beautiful.draw_separator(cr, width, height)
--                         end,
--                         widget = wibox.container.background,
--                     },
--                     layout = wibox.layout.align.vertical,
--                 },
--                 width = calendar_waffle_width,
--                 strategy = "exact",
--                 widget = wibox.container.constraint,
--             },
--             {
--                 {
--                     {
--                         notix.widget,
--                         width = waffle_width,
--                         strategy = "exact",
--                         widget = wibox.container.constraint,
--                     },
--                     left = beautiful.sep_big_size,
--                     draw_empty = false,
--                     widget = fixed_margin,
--                 },
--                 bgimage = function(context, cr, width, height)
--                     width = beautiful.sep_big_size
--                     beautiful.draw_separator(cr, width, height)
--                 end,
--                 widget = wibox.container.background,
--             },
--             layout = wibox.layout.fixed.horizontal,
--         },
--         height = calendar_waffle_width * 2,
--         strategy = "max",
--         widget = wibox.container.constraint,
--     },
--     default_key_handler = function (_self, mod, key, event)
--         if event == "press" then return end
--         if key == "n" then
--             notix.remove_unpinned()
--         end
--     end,
--     on_close = function (_)
--         for s in screen do
--             s.actions.set_clock_area_focus(false)
--         end
--     end,
-- }

local waffle_calendar_source = source.concat{
    upstreams = {
        source.filterable{
            upstream = notix.pinned_container,
            callbacks = {
                filter = function (input, e)
                    for _, t in ipairs{e.notif.app_name, e.notif.title, e.notif.message} do
                        local succ, start = pcall(string.find, t:lower(), input or "")
                        if succ and start then return start end
                    end
                    return false
                end,
                order = function (a, b) if a == b then return nil end return a < b end,
            },
        },
        source.filterable{
            upstream = notix.regular_container,
            callbacks = {
                filter = function (input, e)
                    for _, t in ipairs{e.notif.app_name, e.notif.title, e.notif.message} do
                        local succ, start = pcall(string.find, t:lower(), input or "")
                        if succ and start then return start end
                    end
                    return false
                end,
                order = function (a, b) if a == b then return nil end return a < b end,
            },
        },
        source.filterable{
            upstream = orgenda_items_widget,
            callbacks = {
                filter = function (input, e)
                    local succ, start = pcall(string.find, e.item.text:lower(), input or "")
                    return succ and start
                end,
                order = function (a, b) if a == b then return nil end return a < b end,
            },
        }
    },
}
waffle_calendar_view = bento{
    container = wibox.widget{
        beautiful.apply_border_to_widget_template{
            widget = {
                {
                    {
                        cal_widget,
                        {
                            forced_height = beautiful.sep_big_size,
                            bgimage = function(context, cr, width, height)
                                height = beautiful.sep_big_size
                                beautiful.draw_separator(cr, width, height)
                            end,
                            widget = wibox.container.background,
                        },
                        {
                            {
                                {
                                    {
                                        id = "input_area",
                                        {
                                            {
                                                id = "input_widget",
                                                align = "center",
                                                widget = wibox.widget.textbox,
                                            },
                                            margins = dpi(2),
                                            widget = wibox.container.margin,
                                        },
                                        bg_picker = opicker.concat{opicker.beautiful{"fg_normal"}, "20"},
                                        widget = ocontainer,
                                    },
                                    bottom = dpi(4),
                                    draw_empty = false,
                                    widget = fixed_margin,
                                },
                                {
                                    id = "list_container",
                                    widget = wibox.container.background,
                                },
                                layout = wibox.layout.fixed.vertical,
                            },
                            left = dpi(4),
                            right = dpi(4),
                            bottom = dpi(4),
                            widget = wibox.container.margin,
                        },
                        layout = wibox.layout.fixed.vertical,
                    },
                    height = calendar_waffle_width * 2,
                    width = calendar_waffle_width,
                    strategy = "exact",
                    widget = wibox.container.constraint,
                },
                layout = wibox.layout.fixed.horizontal,
            },
            top = true,
            bottom = true,
            left = true,
            right = true,
        },
        margins = beautiful.useless_gap,
        widget = wibox.container.margin,
    },
    cover = setmetatable(
        {
            -- key_handler = function (_self, mods, key, event)
            --     if key == "n" then
            --         if event == "release" then
            --             notix.remove_unpinned()
            --         end
            --         return true
            --     end
            -- end,
            on_open = function (_self)
                waffle_calendar_view.widget:get_children_by_id("input_area")[1].visible =
                    not waffle:autohide()
            end,
            on_close = function (_)
                for s in screen do
                    s.actions.set_clock_area_focus(false)
                end
            end,
            _readonly = {
                active = true,
                direct_hotkey = false,
            },
        },
        {
            __index = function (self, index)
                return self._readonly[index]
            end,
            __newindex = function (self, index, value)
                if self._readonly[index] ~= nil then return end
                rawset(self, index, value)
            end,
        }),
    source_generator = function ()
        return waffle_calendar_source
    end,
}

-- Settings

waffle_settings_view = view{
    root = decorate_waffle(
        decorate_panel {
            widget = {
                -- button({
                --       markup = "Toggle titlebars",
                --       indicator = em("t"),
                --       key = {"t", "T"},
                --       action = function (alt)
                --          if not alt then
                --             if shared.vars.enable_titlebar then
                --                for _, c in ipairs(capi.client.get()) do
                --                   shared.client.titlebar_disable(c)
                --                end
                --                shared.vars.enable_titlebar = false
                --             else
                --                for _, c in ipairs(capi.client.get()) do
                --                   shared.client.titlebar_enable(c)
                --                end
                --                shared.vars.enable_titlebar = true
                --             end
                --          else
                --              shared.vars.hide_clients_with_titlebars =
                --                  not shared.vars.hide_clients_with_titlebars
                --              capi.client.emit_signal("list")
                --          end
                --          waffle:hide()
                --       end
                -- }),
                -- button({
                --       markup = "Toggle music",
                --       indicator = em("m"),
                --       key = {"m", "M"},
                --       action = function (alt)
                --           music_widget:set_visible(not music_widget:get_visible())
                --           waffle:go_back()
                --       end
                -- }),
                button{
                    markup = "Toggle fortune",
                    indicator = em("f"),
                    key = {"f", "F"},
                    action = function (alt)
                        shared.screen.toggle_fortune()
                    end
                },
                button{
                    markup = "Toggle notes",
                    indicator = em("n"),
                    key = {"n", "N"},
                    action = function (alt)
                        shared.vars.show_notes = not shared.vars.show_notes
                        if not alt then
                            waffle:hide()
                        end
                    end,
                },
                (
                    function()
                        local b
                        b = button{
                            indicator = em("b"),
                            key = {"b", "B"},
                            action = function (alt)
                                for i, v in ipairs(beautiful.bar_styles) do
                                    if v == beautiful.bar_style then
                                        beautiful.bar_style = beautiful.bar_styles[i % #beautiful.bar_styles + 1]
                                        b.label.text = "Cycle bar style: "..beautiful.bar_style
                                        capi.screen.emit_signal("list")
                                        return
                                    end
                                end
                                beautiful.bar_style = "auto"
                                capi.screen.emit_signal("list")
                            end
                        }
                        b.label.text = "Cycle bar style: "..beautiful.bar_style
                        return b
                    end
                )(),
                button{
                    markup = "Wallpaper",
                    indicator = em("w"),
                    key = {"w", "W"},
                    action = function (alt)
                        shared.action.wallpaper_setup()
                        waffle:hide()
                    end
                },
                button{
                    markup = "Reload theme",
                    indicator = em("t"),
                    key = {"t", "T"},
                    action = function (alt)
                        shared.action.reload_theme()
                        waffle:hide()
                    end
                },
                button{
                    markup = "Screen layout",
                    indicator = em("s"),
                    key = {"s", "S"},
                    action = function (alt)
                        if alt then
                            local cmd = {"arandr"}
                            awful.spawn(cmd)
                            waffle:hide()
                        else
                            waffle_root_source_mode = "screen_layout"
                            waffle_dashboard_view.active = false
                            waffle:go_back()
                        end
                    end
                },
                layout = wibox.layout.fixed.vertical,
            }
    }),
}

-- Unused.
waffle_tray_view = view {
    root = decorate_waffle(
        decorate_panel {
            widget = waffle_tray_wrapper
    }),
    on_close = function()
        waffle_tray_wrapper.widget = nil
        shared.screen.attach_tray_widget()
    end
}


local group_cache
local group_label = wibox.widget {
    markup = "group",
    widget = wibox.widget.textbox,
    align = "center",
}

local sticky_cache
local sticky_label = wibox.widget {
    markup = "sticky",
    widget = wibox.widget.textbox,
    align = "center",
}

local above_cache
local above_label = wibox.widget {
    markup = "above",
    widget = wibox.widget.textbox,
    align = "center",
}

local float_cache
local float_label = wibox.widget {
    markup = "float",
    widget = wibox.widget.textbox,
    align = "center",
}

local max_cache
local max_label = wibox.widget {
    markup = "max",
    widget = wibox.widget.textbox,
    align = "center",
}

local min_cache
local min_label = wibox.widget {
    markup = "min",
    widget = wibox.widget.textbox,
    align = "center",
}

local waffle_client_icon_container = wibox.widget {
    forced_width = dpi(40),
    forced_height = dpi(40),
    widget = wibox.container.constraint,
}

local waffle_client_pid_label = wibox.widget{
    align = "center",
    outline_size = dpi(2),
    widget = outlined_textbox,
}

local waffle_client_pid_button = button{
    width = dpi(64),
    height = dpi(64),
    label_widget = wibox.widget{
        nil,
        waffle_client_pid_label,
        {
            {
                text = "d",
                align = "center",
                widget = wibox.widget.textbox
            },
            fg_picker = opicker.branch{
                "highlighted", opicker.beautiful{"special_focus"}},
            widget = ocontainer,
        },
        layout = fixed_align.vertical
    },
    key = {"d", "D"},
    action = function (alt)
        local client = shared.waffle_selected_client
        waffle:hide()
        if not client.valid or type(client.pid) ~= "number" then
            return
        end
        if alt then
            os.execute("echo -n "..tostring(client.pid).." | xclip -i -selection clipboard")
        else
            shared.action.terminal({"htop", "-p", tostring(client.pid)})
        end
    end
}

local close_label = wibox.widget {
    markup = "Close",
    widget = wibox.widget.textbox,
    align = "center",
}

local update_client_waffle_labels
local client_waffle_attached = false
local client_waffle_transient = false
local client_close_count
local client_waffle = view {
    root = decorate_waffle {
        decorate_panel {
            widget = {
                {
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = group_label,
                            indicator = em("g"),
                            key = {"g", "G"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                shared.client.toggle_grouping(client)
                            end
                    }),
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = sticky_label,
                            indicator = em("s"),
                            key = {"s", "S"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.sticky = not client.sticky
                            end
                    }),
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = above_label,
                            indicator = em("a"),
                            key = {"a", "A"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.above = not client.above
                            end
                    }),
                    layout = wibox.layout.fixed.horizontal,
                },
                {
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = min_label,
                            indicator = em("i"),
                            key = {"i", "I"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if client.valid and not client.minimized then
                                    client_waffle_attached = false
                                end
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.minimized = not client.minimized
                            end
                    }),
                    {
                        {
                            waffle_client_icon_container,
                            forced_height = dpi(64),
                            forced_width = dpi(64),
                            halign = "center",
                            valign = "center",
                            widget = wibox.container.place,
                        },
                        waffle_client_pid_button,
                        layout = wibox.layout.stack,
                    },
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = max_label,
                            indicator = em("m"),
                            key = {"m", "M"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.maximized = not client.maximized
                            end
                    }),
                    layout = wibox.layout.fixed.horizontal,
                },
                {
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            label_widget = float_label,
                            indicator = em("f"),
                            key = {"f", "F"},
                            action = function (alt)
                                local client = shared.waffle_selected_client
                                if not alt then
                                    waffle:hide()
                                end
                                if not client.valid then
                                    return
                                end
                                client.floating = not client.floating
                            end
                    }),
                    button({
                            width = dpi(64),
                            height = dpi(64),
                            button_layout = fixed_align.vertical,
                            markup = "Pos",
                            indicator = em("p"),
                            key = {"p", "P"},
                            key_action = function (alt)
                                local client = shared.waffle_selected_client
                                waffle:hide()
                                if not client.valid then
                                    return
                                end
                                shared.client.start_switcher(client, false)
                            end,
                            buttons = awful.util.table.join(
                                awful.button({ }, 1, function ()
                                                 local c = shared.waffle_selected_client
                                                 waffle:hide()
                                                 if not c.valid then
                                                     return
                                                 end
                                                 c:raise()
                                                 c.maximized = false
                                                 c.minimized = false
                                                 local geo = c:geometry()
                                                 mouse.coords({ x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 })
                                                 awful.mouse.client.move(c)
                                             end),
                                awful.button({ }, 3, function ()
                                                 local c = shared.waffle_selected_client
                                                 waffle:hide()
                                                 if not c.valid then
                                                     return
                                                 end
                                                 c:raise()
                                                 c.maximized = false
                                                 c.minimized = false
                                                 awful.mouse.client.resize(c, "bottom_right")
                                             end)),
                    }),
                    button({
                               width = dpi(64),
                               height = dpi(64),
                               button_layout = fixed_align.vertical,
                               label_widget = close_label,
                               indicator = em("c"),
                               key = {"c", "C"},
                               action = function (alt)
                                   local client = shared.waffle_selected_client
                                   if not client.valid then
                                       waffle:hide()
                                       return
                                   end
                                   client_close_count = client_close_count + 1
                                   if client_close_count >= 3 then
                                       waffle:hide()
                                       client:kill()
                                   else
                                       update_client_waffle_labels()
                                   end
                               end,
                           }),
                    layout = wibox.layout.fixed.horizontal,
                },
                layout = wibox.layout.fixed.vertical,
            },
        },
        spacing = button_padding,
        layout = wibox.layout.fixed.vertical,
    },
    on_open = function()
        if client_waffle_attached and not client_waffle_transient then
            awful.client.focus.history.disable_tracking()
        end
        client_waffle_transient = false
        client_close_count = 0
    end,
    on_close = function()
        if client_waffle_attached and not client_waffle_transient then
            awful.client.focus.history.enable_tracking()
            local c = shared.waffle_selected_client
            client_waffle_attached = false
            gtimer.delayed_call(function () c:emit_signal("request::activate", "switch", {raise=true}) end)
        end
        client_waffle_transient = false
        -- TODO check why I did this.
        -- if shared.waffle_selected_client and shared.waffle_selected_client.valid then
        --     shared.waffle_selected_client:emit_signal("property::name")
        -- end
        shared.waffle_selected_client = nil
    end,
    key_filter = function (_self, mod, key, event)
        for i = 1, #mod do
            if mod[i] == "Control" then return false end
        end
        return true
    end,
    default_key_handler = function (_self, mod, key, event)
        if event ~= "press" then return end
        if key == "Return" then
            client_waffle_attached = true
            waffle:hide()
            return
        end
        if key:find("^XF86Launch.*") then
            waffle:hide()
            return
        end
        for i = 1, #mod do mod[mod[i]] = true end
        if key == "Left" or key == "Right" or key == "Up" or key == "Down" then
            if not client_waffle_attached then return end
            local c = shared.waffle_selected_client

            if mod["Shift"] then
                -- Switch screen by direction
                awful.screen.focus_bydirection(key:lower(), c.screen)
                c:move_to_screen(capi.mouse.screen.index)
                c:emit_signal("request::activate", "mouse.resize", {raise = true})

                client_waffle_transient = true
                waffle:hide()

                c:raise()
                client_waffle_transient = true
                capi.awesome.emit_signal("show_client_waffle", c, "client")
            else
                -- Focus by direction
                if c.maximized then return end

                -- Filter out maximized clients.
                -- This would only work single-threaded.
                local old_visible = awful.client.visible
                awful.client.visible = function (...)
                    local result = old_visible(...)
                    local head = 0
                    for i = 1, #result do
                        if not result[i].maximized then
                            head = head + 1
                            result[head] = result[i]
                        end
                    end
                    for i = #result, head + 1, -1 do
                        result[i] = nil
                    end
                    return result
                end
                pcall(awful.client.focus.global_bydirection, key:lower(), c)
                awful.client.visible = old_visible

                c = capi.client.focus
                if c then
                    client_waffle_transient = true
                    waffle:hide()

                    c:raise()
                    client_waffle_transient = true
                    capi.awesome.emit_signal("show_client_waffle", c, "client")
                end
            end
            return
        elseif key == "Prior" or key == "Next" or key == "," or key == "." then
            if not client_waffle_attached then return end

            local c = shared.waffle_selected_client
            local clients = c.screen.tasklist_clients()

            local forward = key == "Prior" or key == ","
            for i = 1, #clients do
                if clients[i] == c then
                    if forward and i > 1 then
                        c = clients[i - 1]
                    elseif not forward and i < #clients then
                        c = clients[i + 1]
                    else
                        c = nil
                    end
                    break
                end
            end

            if c then
                client_waffle_transient = true
                waffle:hide()

                c:raise()
                client_waffle_transient = true
                capi.awesome.emit_signal("show_client_waffle", c, "client")
            end
            return
        end
    end,
}

function shared.waffle.show_client_waffle(c, args)
    args = args or {}
    if shared.waffle_selected_client ~= nil then
        waffle:hide()
    end
    if args.anchor == "client" then
        local geo = c:geometry()
        args.anchor = { x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 }
        args.screen = c.screen
        client_waffle_attached = true
    end
    waffle:show(client_waffle, args)
    shared.waffle_selected_client = c
    waffle_client_icon_container.widget = wibox.widget{
        awful.widget.clienticon(c),
        {
            id = "default_icon",
            image = beautiful.client_default_icon,
            widget = masked_imagebox,
        },
        widget = fallback,
    }
    if c.pid then
        waffle_client_pid_label.text = c.pid
        waffle_client_pid_button.visible = true
    else
        waffle_client_pid_button.visible = false
    end
    update_client_waffle_labels()
    capi.client.emit_signal("list")
end

capi.awesome.connect_signal(
    "show_client_waffle",
    function (client, anchor)
        shared.waffle.show_client_waffle(client, {anchor = anchor})
    end
)

update_client_waffle_labels = function ()
    if not waffle:is_in_view(client_waffle) then
        return
    end

    local now_group = (shared.waffle_selected_client and shared.waffle_selected_client.cgroup ~= nil)
    if group_cache ~= now_group then
        group_cache = now_group
        group_label:set_markup(now_group and "GROUP" or "group")
    end

    local now_sticky = (shared.waffle_selected_client and shared.waffle_selected_client.sticky)
    if sticky_cache ~= now_sticky then
        sticky_cache = now_sticky
        sticky_label:set_markup(now_sticky and "STICKY" or "sticky")
    end

    local now_above = (shared.waffle_selected_client and shared.waffle_selected_client.above)
    if above_cache ~= now_above then
        above_cache = now_above
        above_label:set_markup(now_above and "ABOVE" or "above")
    end

    local now_float = (shared.waffle_selected_client and shared.waffle_selected_client.floating)
    if float_cache ~= now_float then
        float_cache = now_float
        float_label:set_markup(now_float and "FLOAT" or "float")
    end

    local now_max = (shared.waffle_selected_client and shared.waffle_selected_client.maximized)
    if max_cache ~= now_max then
        max_cache = now_max
        max_label:set_markup(now_max and "MAX" or "max")
    end

    local now_min = (shared.waffle_selected_client and shared.waffle_selected_client.minimized)
    if min_cache ~= now_min then
        min_cache = now_min
        min_label:set_markup(now_min and "MIN" or "min")
    end

    if client_close_count == 0 then
        close_label:set_markup("Close")
    else
        close_label:set_markup("Close "..tostring(client_close_count))
    end
end

client.connect_signal("property::cgroup", update_client_waffle_labels)
client.connect_signal("property::sticky", update_client_waffle_labels)
client.connect_signal("property::above", update_client_waffle_labels)
client.connect_signal("property::floating", update_client_waffle_labels)
client.connect_signal("property::maximized", update_client_waffle_labels)
client.connect_signal("property::minimized", update_client_waffle_labels)

return nil

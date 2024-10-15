local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local cwidget = require((...):match("(.-)[^%.]+$") .. "widget")
local csource = require((...):match("(.-)[^%.]+$") .. "source")
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
local glib = require("lgi").GLib
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
local prompt_caret_textbox = require("prompt_caret_textbox")
local debug_container = require("debug_container")
local masked_imagebox = require("masked_imagebox")
local acolor = require("aux").color
-- local mpc_gobject = require("mpc-gobject")
local orgenda = require("orgenda")
local scroller = require("scroller")
local fts = require("hotpot").focus_timestamp
local dpi = require("beautiful.xresources").apply_dpi
local prism = require("prism")
local bento = require("awemni.bento")
local notix = require("notix")
local taglist = require("config.taglist")

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

local function aggregate_key_handlers(args)
    local widget = args.widget
    assert(widget ~= nil)
    local checked = {[widget] = true}
    local traverse_pool = {widget}
    local keys = {}
    if args.keys then
        for k, v in pairs(args.keys) do
            keys[k] = v
        end
    end

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
                if not keys[k] then
                    keys[k] = f
                end
            end
        end
    end

    return function (mods, key, event)
        if args.handle_key then
            return args.handle_key(mods, key, event)
        end
        if args.key_filter and not args.key_filter(mods, key, event) then
            return false
        elseif keys[key] then
            keys[key](mods, key, event)
            return true
        elseif args.handle_key_fallback then
            return args.handle_key_fallback(mods, key, event)
        else
            return false
        end
    end
end

local function simple_button(args)
    local button_action = args.button_action or args.action
    local key_action = args.key_action or args.action

    local ret = wibox.widget{
        {
            {
                {
                    {
                        args.widget,
                        buttons = args.buttons,
                        margins = button_padding,
                        widget = wibox.container.margin,
                    },
                    width = args.width,
                    height = args.height,
                    strategy = "exact",
                    widget = wibox.container.constraint,
                },
                layout_pickers = {
                    top = prism.picker.branch{"active", dpi(2), 0},
                    bottom = prism.picker.branch{"active", -dpi(2), 0},
                    left = prism.picker.branch{"active", dpi(2), 0},
                    right = prism.picker.branch{"active", -dpi(2), 0},
                },
                widget = prism.container.margin,
            },
            draw_pickers = {
                fg = prism.picker.beautiful{
                    "fg_", prism.picker.switch{
                        {"active", "focus"},
                        {"hover", "focus"},
                        default = "normal",
                    },
                },
                prism.picker.list{"bg", prism.picker.beautiful{
                                      "bg_", prism.picker.switch{
                                          {"active", "focus"},
                                          {"hover", "focus"},
                                      },
                                  }},
            },
            widget = prism.container.background,
        },
        widget = prism.layer,
    }

    local function update_context_transformation()
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
            update_context_transformation()
        end
    )

    ret:connect_signal(
        "mouse::leave",
        function ()
            ret.hover = nil
            ret.active = nil
            update_context_transformation()
        end
    )

    ret:connect_signal(
        "button::press",
        function ()
            ret.active = true
            update_context_transformation()
        end
    )

    ret:connect_signal(
        "button::release",
        function (_widget, _x, _y, button)
            if ret.active and button_action and (button == 1 or button == 3) then
                button_action(button ~= 1)
            end
            ret.active = nil
            update_context_transformation()
        end
    )

    update_context_transformation()

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
        wibox.widget{
                markup = args.markup,
                text = args.text,
                font = font_normal,
                forced_height = button_height,
                align = "center",
                valign = "center",
                widget = wibox.widget.textbox,
        }

    args.width = args.width or waffle_width

    local icon_widget = args.icon_widget or (
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
    )
    args.widget = wibox.widget{
        icon_widget and {
            icon_widget,
            right = args.button_layout and 0 or button_padding,
            widget = wibox.container.margin,
        },
        label,
        args.indicator and {
            {
                {
                    text = args.indicator,
                    font = font_normal,
                    align = "center",
                    valign = "center",
                    widget = wibox.widget.textbox,
                },
                left = args.button_layout and 0 or button_padding,
                widget = wibox.container.margin,
            },
            draw_pickers = {
                fg = prism.picker.beautiful{"special_", prism.picker.highlighted_switcher},
            },
            widget = prism.container.background,
        },
        layout = args.button_layout or fixed_align.horizontal,
    }

    local ret = simple_button(args)
    ret.label = label

    return ret
end

local sep_color = gcolor(acolor(beautiful.fg_normal):blend_with(acolor(beautiful.bg_normal), 0.75):to_string())
local function decorate_panel(args)
    if args.top_sep then
        return wibox.widget{
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
        return args.widget
    end
end

local function decorate_waffle(widget)
    return wibox.widget{
        beautiful.apply_border_to_widget{
            widget = widget,
            top = true,
            bottom = true,
            left = true,
            right = true,
        },
        margins = beautiful.useless_gap,
        widget = wibox.container.margin,
    }
end

--------------------------------------------------------------------------------
-- Main waffles
--------------------------------------------------------------------------------

local waffle_shutdown_view = decorate_waffle(
    decorate_panel{
        widget = wibox.widget{
            button{
                -- icon = icons.sleep,
                markup = "Suspend",
                indicator = em("s"),
                key = {"s", "S"},
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "suspend"}, false)
                end
            },
            button {
                -- icon = icons.sleep,
                markup = "Hibernate",
                indicator = em("h"),
                key = {"h", "H"},
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "hibernate"}, false)
                end
            },
            button {
                markup = "Reboot",
                indicator = em("r"),
                key = {"r", "R"},
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "reboot"}, false)
                end
            },
            button {
                -- icon = icons.poweroff,
                markup = "Power off",
                indicator = em("p"),
                key = {"p", "P"},
                action = function (alt)
                    waffle:hide()
                    awful.spawn({"systemctl", "poweroff"}, false)
                end
            },
            layout = wibox.layout.fixed.vertical
        }
    }
)
local waffle_shutdown_view_handle_key = aggregate_key_handlers{
    widget = waffle_shutdown_view
}
function waffle_shutdown_view:handle_key(mods, key, event)
    return waffle_shutdown_view_handle_key(mods, key, event)
end

local waffle_settings_view = decorate_waffle(
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
                markup = "Toggle tasklist brief mode",
                indicator = em("i"),
                key = {"i", "I"},
                action = function (alt)
                    beautiful.set_tasklist_brief(not beautiful.tasklist_brief)
                    capi.screen.emit_signal("list")
                end
            },
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
                    local get_label = function ()
                        return "Notification: "..(notix.config.silent and "silent" or "show")
                    end
                    local b
                    b = button{
                        indicator = em("o"),
                        key = {"o", "O"},
                        action = function (alt)
                            notix.config.silent = not notix.config.silent
                            b.label.text = get_label()
                        end
                    }
                    b.label.text = get_label()
                    return b
                end
            )(),
            (
                function()
                    local b
                    b = button{
                        indicator = em("r"),
                        key = {"r", "R"},
                        action = function (alt)
                            beautiful.set_bar_rows(({2, 1})[beautiful.bar_rows or 1])
                            b.label.text = "Bar rows: "..beautiful.bar_rows
                            capi.screen.emit_signal("list")
                        end
                    }
                    b.label.text = "Bar rows: "..beautiful.bar_rows
                    return b
                end
            )(),
            (
                function()
                    local b
                    local pos_cycle = {"bottom", "right", "top", "left"}
                    b = button{
                        indicator = em("p"),
                        key = {"p", "P"},
                        action = function (alt)
                            for i, p in ipairs(pos_cycle) do
                                if p == beautiful.bar_position then
                                    print(p, pos_cycle[i % 4 + 1])
                                    beautiful.set_bar_position(pos_cycle[i % 4 + 1])
                                    break
                                end
                            end
                            b.label.text = "Bar position: "..beautiful.bar_position
                            capi.screen.emit_signal("list")
                        end
                    }
                    b.label.text = "Bar position: "..beautiful.bar_position
                    return b
                end
            )(),
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
                        root_bentobox_open("screen_layout")
                        waffle:go_back()
                    end
                end
            },
            layout = wibox.layout.fixed.vertical,
        }
    })
local waffle_settings_view_handle_key = aggregate_key_handlers{
    widget = waffle_settings_view
}
function waffle_settings_view:handle_key(mods, key, event)
    return waffle_settings_view_handle_key(mods, key, event)
end

-- -- local battery_widget_width = waffle_width - button_height - button_padding * 3
-- -- local battery_widget
-- -- do
-- --    local charging_color = acolor.from_string(beautiful.special_normal):blend_with(beautiful.bg_normal, 0.25):to_string()
-- --    local background_color = beautiful.border_normal

-- --    local battery_status_widget = wibox.widget {
-- --         text = "",
-- --         ellipsize = "end",
-- --         align = "center",
-- --         forced_width = battery_widget_width,
-- --         forced_height = button_height - dpi(2),
-- --         font = font_info,
-- --         widget = wibox.widget.textbox
-- --     }

-- --    local battery_percentage_widget = wibox.widget {
-- --       max_value = 1,
-- --       forced_width = battery_widget_width,
-- --       forced_height = dpi(2),
-- --       paddings = 0,
-- --       border_width = 0,
-- --       background_color = "#00000000",
-- --       shape = gshape.bar,
-- --       clip = true,
-- --       widget = wibox.widget.progressbar
-- --    }

-- --    battery_widget = wibox.widget {
-- --        {
-- --            image = icons.battery_full,
-- --            forced_height = button_height,
-- --            forced_width = button_height,
-- --            widget = wibox.widget.imagebox
-- --        },
-- --        {
-- --            battery_status_widget,
-- --            battery_percentage_widget,
-- --            layout = wibox.layout.fixed.vertical
-- --        },
-- --        spacing = button_padding,
-- --        visible = false,
-- --        layout = wibox.layout.fixed.horizontal
-- --    }

-- --    -- Surface-linux
-- --    local battery_status_command = {"mshw0084-rqst.py", "-q", "-d", "/dev/ttyS0", "bat1.pretty"}
-- --    -- -- For debugging
-- --    -- local battery_status_command = {"echo", "Percentage: 70%\nState:Charging\nRemaining: 10 hours"}

-- --    local function parse_battery_output(stdout)
-- --        local results = {}
-- --        for line in stdout:gmatch("[^\r\n]+") do
-- --            key, value = line:match("%s*([^:]-)%s*:%s*(.-)%s*$")
-- --            if key == "Percentage" then
-- --                results.value = tonumber(value:match("(%d*)%%")) / 100
-- --            elseif key == "Remaining" then
-- --                results.remaining = value
-- --            elseif key == "State" then
-- --                results.state = value
-- --                results.charging = (value:match("Charging") ~= nil)
-- --            end
-- --        end
-- --        return results
-- --    end

-- --    local update_graphic = function (widget, stdout)
-- --        local status = parse_battery_output(stdout)
-- --        battery_widget.visible = true
-- --        battery_percentage_widget.value = status.value
-- --        battery_percentage_widget.color = status.charging and charging_color or nil
-- --        battery_status_widget:set_text(status.state..": "..status.remaining)
-- --    end

-- --    local function spawn_and_update_battery(cmd)
-- --        awful.spawn.easy_async_with_shell(
-- --            battery_status_command,
-- --            function (stdout, stderr, exitreason, exitcode)
-- --                update_graphic(battery_widget, stdout, stderr, exitreason, exitcode)
-- --            end
-- --        )
-- --    end

-- --    gtimer {
-- --        timeout = 60,
-- --        call_now = true,
-- --        autostart = true,
-- --        callback = function ()
-- --            awful.spawn.easy_async(
-- --                battery_status_command,
-- --                function (stdout)
-- --                    update_graphic(battery_widget, stdout)
-- --                end
-- --            )
-- --        end,
-- --    }
-- -- end

-- -- local music_widget
-- -- do
-- --     -- Not used anymore
-- --     -- local GET_STATUS_CMD = 'mpc current -f "%title% - %artist%" -q'
-- --     -- local NEXT_CMD = 'mpc next -q'
-- --     -- local PREV_CMD = 'mpc prev -q'
-- --     -- local TOGGLE_CMD = 'mpc toggle -q'

-- --     local mpd_status_widget = wibox.widget {
-- --         text = "",
-- --         forced_height = button_height,
-- --         font = font_normal,
-- --         -- outline_size = dpi(2),
-- --         -- widget = outlined_textbox
-- --         widget = wibox.widget.textbox
-- --     }

-- --     local mpd_title_widget = wibox.widget {
-- --         text = "",
-- --         ellipsize = "end",
-- --         forced_height = button_height,
-- --         widget = wibox.widget.textbox
-- --     }

-- --     local mpd_meta_widget = wibox.widget {
-- --         text = "",
-- --         ellipsize = "end",
-- --         font = font_info,
-- --         forced_height = button_height,
-- --         widget = wibox.widget.textbox
-- --     }

-- --     local mpd_progress_widget = wibox.widget {
-- --         max_value     = 1,
-- --         value         = 0,
-- --         forced_height = dpi(2),
-- --         background_color = "#00000000",
-- --         widget        = wibox.widget.progressbar,
-- --     }

-- --     -- local lighter_fg_normal = acolor.from_string(beautiful.fg_normal):blend_with(beautiful.bg_normal, 0.75):to_string()
-- --     -- local lighter_fg_focus = acolor.from_string(beautiful.fg_focus):blend_with(beautiful.bg_focus, 0.75):to_string()

-- --     local mpd_icon_widget =
-- --         wibox.widget {
-- --             {
-- --                 image = icons.music,
-- --                 resize = true,
-- --                 forced_width = button_height,
-- --                 forced_height = button_height,
-- --                 widget = masked_imagebox,
-- --             },
-- --             fg_picker = opicker.beautiful{
-- --                 "fg_", opicker.highlighted_switcher},
-- --             widget = ocontainer,
-- --         }

-- --     mpc_gobject:connect_signal(
-- --         "update::status",
-- --         function (_, status)
-- --             if status.err then
-- --                 -- mpd_status_widget:set_text("✖")
-- --                 -- mpd_title_widget:set_text(tostring(err))
-- --                 -- mpd_meta_widget:set_text("(╯°Д°)╯ ┻━┻")
-- --                 music_widget:set_visible(false)
-- --                 return
-- --             end

-- --             if status.state == "play" then
-- --                 mpd_status_widget:set_text("▶")
-- --             elseif status.state == "pause" then
-- --                 mpd_status_widget:set_text("⏸")
-- --             elseif status.state == "stop" then
-- --                 mpd_status_widget:set_text("⏹")
-- --             else
-- --                 mpd_status_widget:set_text(status.state)
-- --             end

-- --             mpd_progress_widget:set_value(status.progress or 0)
-- --             music_widget:set_visible(true)
-- --         end
-- --     )

-- --     mpc_gobject:connect_signal(
-- --         "update::song",
-- --         function (_, song)
-- --             mpd_title_widget:set_text(song.title or "")

-- --             local meta = {}
-- --             if song.artist then
-- --                 meta[#meta + 1] = song.artist
-- --             end
-- --             if song.album then
-- --                 meta[#meta + 1] = song.album
-- --             end

-- --             mpd_meta_widget:set_text(table.concat(meta, " - "))
-- --         end
-- --     )

-- --     music_widget = button {
-- --         icon_widget = wibox.widget {
-- --             {
-- --                 mpd_status_widget,
-- --                 widget = wibox.container.place
-- --             },
-- --             {
-- --                 mpd_icon_widget,
-- --                 widget = wibox.container.place
-- --             },
-- --             layout = wibox.layout.fixed.vertical,
-- --         },
-- --         label_widget = wibox.widget {
-- --             {
-- --                 {
-- --                     {
-- --                         {
-- --                             mpd_title_widget,
-- --                             speed = 100,
-- --                             step_function = wibox.container.scroll.step_functions
-- --                                 .waiting_nonlinear_back_and_forth,
-- --                             layout = wibox.container.scroll.horizontal,
-- --                         },
-- --                         widget = wibox.container.place
-- --                     },
-- --                     mpd_progress_widget,
-- --                     {
-- --                         {
-- --                             {
-- --                                 mpd_meta_widget,
-- --                                 speed = 100,
-- --                                 step_function = wibox.container.scroll.step_functions
-- --                                     .waiting_nonlinear_back_and_forth,
-- --                                 layout = wibox.container.scroll.horizontal,
-- --                             },
-- --                             widget = wibox.container.place
-- --                         },
-- --                         {
-- --                             text = "_(:3」∠)_",
-- --                             align = "center",
-- --                             force_height = button_height - dpi(2),
-- --                             font = font_info,
-- --                             widget = wibox.widget.textbox
-- --                         },
-- --                         widget = fallback
-- --                     },
-- --                     layout = wibox.layout.fixed.vertical,
-- --                 },
-- --                 draw_empty = false,
-- --                 left = button_padding,
-- --                 right = button_padding,
-- --                 widget = fixed_margin,
-- --             },
-- --             {
-- --                 text = "Music",
-- --                 align = "center",
-- --                 valign = "center",
-- --                 forced_height = button_height,
-- --                 widget = wibox.widget.textbox,
-- --             },
-- --             widget = fallback,
-- --         },
-- --         indicator = em("m"),
-- --         key = {"m", "M"},
-- --         action = function (alt)
-- --             waffle:hide()
-- --             shared.action.music_app()
-- --         end,
-- --         buttons = awful.util.table.join(
-- --             awful.button({ }, 1, function () waffle:hide(); shared.action.music_app() end),
-- --             awful.button({ }, 3, function () mpc_gobject:toggle_play() end),
-- --             awful.button({ }, 4, function ()
-- --                     mpc_gobject:go_next()
-- --             end),
-- --             awful.button({ }, 5, function ()
-- --                     mpc_gobject:go_previous()
-- --             end)
-- --         ),
-- --     }

-- --     music_widget.keys["Left"] = function (mod, _key, event)
-- --         if event == "press" then return end
-- --         for _, m in ipairs(mod) do mod[m] = true end
-- --         if mod["Shift"] then
-- --             mpc_gobject:go_previous()
-- --         end
-- --     end
-- --     music_widget.keys["Right"] = function (mod, _key, event)
-- --         if event == "press" then return end
-- --         for _, m in ipairs(mod) do mod[m] = true end
-- --         if mod["Shift"] then
-- --             mpc_gobject:go_next()
-- --         end
-- --     end
-- --     music_widget.keys["Up"] = function (mod, _key, event)
-- --         if event == "press" then return end
-- --         for _, m in ipairs(mod) do mod[m] = true end
-- --         if mod["Shift"] then
-- --             mpc_gobject:toggle_play()
-- --         end
-- --     end
-- -- end

local waffle_root_view
local waffle_root_indicator
local waffle_root_source_mode
local waffle_root_bento
local waffle_root_prompt
local waffle_root_bentobox
local waffle_root_bentobox_open
local waffle_dashboard_widget

local todo_text = wibox.widget{
    align = "center",
    widget = wibox.widget.textbox,
}
orgenda.data:connect_signal(
    "property::items",
    function (_, path, items)
        local todo_count = 0
        for _, item in ipairs(orgenda.data.items) do
            if not item.done then
                todo_count = todo_count + 1
            end
        end
        if todo_count > 0 then
            todo_text.text = tostring(todo_count).." TODO"..
                (todo_count > 1 and "s" or "")
        else
            todo_text.text = "No TODOs!"
        end
    end
)

local function get_avatar()
    local user_avatar_path = os.getenv("HOME").."/Pictures/avatar.svg"
    if gfs.file_readable(user_avatar_path) then
        return {
            {
                image = user_avatar_path,
                widget = masked_imagebox,
            },
            height = button_height * 2,
            widget = wibox.container.constraint,
        }
    end
    return {
        {
            image = icons.calendar_todo,
            widget = masked_imagebox,
        },
        height = button_height,
        widget = wibox.container.constraint,
    }
end

local waffle_dashboard_header_widget = decorate_panel{
    widget = {
        button{
            label_widget = wibox.widget{
                {
                    {
                        get_avatar(),
                        right = beautiful.sep_small_size,
                        widget = wibox.container.margin,
                    },
                    widget = wibox.container.place,
                },
                {
                    {
                        {
                            format = "%Y-%m-%d %a %H:%M",
                            widget = wibox.widget.textclock,
                        },
                        widget = wibox.container.place,
                    },
                    {
                        todo_text,
                        widget = wibox.container.place,
                    },
                    layout = wibox.layout.flex.vertical,
                },
                nil,
                widget = fixed_align.horizontal,
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
        layout = wibox.layout.fixed.vertical,
    },
}

local waffle_dashboard_status_widget = decorate_panel{
    top_sep = true,
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

-- local
function root_bentobox_open(mode)
    waffle_root_source_mode = mode
    if waffle_root_bentobox.visible then
        waffle_root_bento:reload_source()
    else
        waffle_root_bentobox.visible = true
        waffle_root_bento:open()
    end
end

local function root_bentobox_close()
    if waffle_root_bentobox.visible then
        waffle_root_bento:close()
        waffle_root_bentobox.visible = false
    end
end

local function root_bentobox_is_active()
    return waffle_root_bentobox.visible
end

local grid_height = dpi(48)
local grid_width = waffle_width / 3
local grid_button_layout = fixed_align.vertical
local waffle_dashboard_action_grid_widget = decorate_panel{
    top_sep = true,
    widget = wibox.widget{
        {
            button{
                height = grid_height,
                width = grid_width,
                button_layout = grid_button_layout,
                label_widget = icon_label(icons.launcher),
                indicator = em("r"),
                key = {"r", "R", " "},
                action = function (alt)
                    if waffle:is_in_view(waffle_root_view) then
                        root_bentobox_open(alt and "zsh" or "apps")
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
                    if waffle:is_in_view(waffle_root_view) then
                        root_bentobox_open("tray")
                    end
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

local playerctl_button = button{
    label_widget = wibox.widget{
        cwidget.playerctl_widget,
        height = button_height * 2,
        widget = wibox.container.constraint,
    },
    indicator = em("m"),
}
playerctl_button.visible = cwidget.playerctl_widget.visible
cwidget.playerctl_widget:connect_signal(
    "property::visible", function ()
        playerctl_button.visible = cwidget.playerctl_widget.visible
    end)

local waffle_dashboard_action_list_widget = decorate_panel {
    top_sep = true,
    widget = {
        playerctl_button,
        button {
            icon = icons.audio,
            label_widget = wibox.widget{
                {
                    cwidget.audio_sink_widget,
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
                    root_bentobox_open("audio_sink")
                end
            end,
        },
        button {
            icon = icons.mic,
            label_widget = wibox.widget{
                {
                    cwidget.audio_source_widget,
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
                    root_bentobox_open("audio_source")
                end
            end,
        },
        layout = wibox.layout.fixed.vertical,
    },
}

waffle_dashboard_widget = wibox.widget{
    waffle_dashboard_header_widget,
    waffle_dashboard_status_widget,
    waffle_dashboard_action_grid_widget,
    waffle_dashboard_action_list_widget,
    layout = wibox.layout.fixed.vertical,
}

local tag_keys = {}
for i = 1, #shared.screen.tags do
    local key = tostring(i)
    tag_keys[key] = i
end
local waffle_dashboard_handle_key = aggregate_key_handlers{
    widget = waffle_dashboard_widget,
    key_filter = function (mod, key, event)
        if event ~= "press" then return true end
        if key:find("^XF86Launch.*") then
            waffle:hide()
            return false
        end
        if (key == "Left" or key == "Right" or key == "Up" or key == "Down") then
            pcall(awful.screen.focus_bydirection, key:lower())
            waffle:hide()
            capi.awesome.emit_signal("show_main_waffle", {anchor = "screen"})
            return false
        end
        if tag_keys[key] ~= nil then
            -- Support other tag operation?
            taglist.switch_or_restore(awful.screen.focused().tags[tag_keys[key]])
            return false
        end
        return true
    end
}

local function get_source()
    waffle_root_prompt.text = ""
    if waffle_root_source_mode == "audio_sink" then
        waffle_root_indicator.image = icons.audio
        return csource.get_audio_sinks()
    elseif waffle_root_source_mode == "audio_source" then
        waffle_root_indicator.image = icons.mic
        return csource.get_audio_sources()
    elseif waffle_root_source_mode == "screen_layout" then
        waffle_root_indicator.image = icons.monitor
        return csource.get_screen_layouts()
    elseif waffle_root_source_mode == "tray" then
        waffle_root_indicator.image = icons.dots
        return csource.get_tray_items()
    elseif waffle_root_source_mode == "zsh" then
        waffle_root_indicator.image = icons.terminal
        return csource.get_zsh_completion(waffle_root_bento, waffle_root_prompt)
    end
    waffle_root_indicator.image = icons.launcher
    return csource.get_apps()
end

local waffle_root_input_widget = wibox.widget{
    align = "center",
    ellipsize = "none",
    widget = prompt_caret_textbox,
}
-- local
waffle_root_bento = bento{
    input_widget = waffle_root_input_widget,
    source_generator = get_source,
}
waffle_root_view = wibox.widget{
    waffle_dashboard_widget,
    {
        id = "bentobox",
        visible = false,
        {
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
                                        {
                                            id = "prompt",
                                            widget = wibox.widget.textbox,
                                        },
                                        height = button_height,
                                        strategy = "exact",
                                        widget = wibox.container.constraint,
                                    },
                                    widget = wibox.layout.fixed.horizontal,
                                },
                                {
                                    id = "input_area",
                                    {
                                        {
                                            waffle_root_input_widget,
                                            fill_horizontal = true,
                                            content_fill_horizontal = true,
                                            widget = wibox.container.place,
                                        },
                                        margins = dpi(2),
                                        widget = wibox.container.margin,
                                    },
                                    draw_pickers = {
                                        bg = prism.picker.concat{prism.picker.beautiful{"fg_normal"}, "20"},
                                    },
                                    widget = prism.container.background,
                                },
                                layout = fixed_align.horizontal,
                            },
                            widget = wibox.container.place,
                        },
                        {
                            {
                                waffle_root_bento,
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
                draw_pickers = {
                    fg = prism.picker.beautiful{"fg_normal"},
                },
                widget = prism.container.background,
            },
            width = dpi(500),
            strategy = "exact",
            widget = wibox.container.constraint,
        },
        content_fill_vertical = true,
        vertical_fit = false,
        widget = fixed_place,
    },
    layout = wibox.layout.fixed.horizontal,
}
-- local
waffle_root_bentobox = waffle_root_view:get_children_by_id("bentobox")[1]
waffle_root_indicator = waffle_root_view:get_children_by_id("source_indicator")[1]
waffle_root_prompt = waffle_root_view:get_children_by_id("prompt")[1]
waffle_root_view = decorate_waffle(decorate_panel{widget = waffle_root_view})
function waffle_root_view:handle_key(mods, key, event)
    if root_bentobox_is_active() then
        return waffle_root_bento:handle_key(mods, key, event)
    end
    return waffle_dashboard_handle_key(mods, key, event)
end
function waffle_root_view:handle_back()
    if root_bentobox_is_active() then
        root_bentobox_close()
        return true
    end
    return false
end
function waffle_root_view:handle_close()
    root_bentobox_close()
end
function shared.waffle.launcher()
    waffle:show(nil, {anchor = "screen"})
    root_bentobox_open("apps")
end

function shared.waffle.zsh_completion()
    waffle:show(nil, {anchor = "screen"})
    root_bentobox_open("zsh")
end

waffle:set_root_view(waffle_root_view)

capi.awesome.connect_signal(
    "show_main_waffle",
    function (args)
        waffle:show(nil, args)
    end
)

-- -- Unused.
-- waffle_tray_view = view {
--     root = decorate_waffle(
--         decorate_panel {
--             widget = waffle_tray_wrapper
--     }),
--     on_close = function()
--         waffle_tray_wrapper.widget = nil
--         shared.screen.attach_tray_widget()
--     end
-- }

--------------------------------------------------------------------------------
-- Calendar/todo/notification
--------------------------------------------------------------------------------

local waffle_calendar_input_widget = wibox.widget{
    align = "center",
    ellipsize = "none",
    widget = prompt_caret_textbox,
}
local waffle_calendar_bento = bento{
    input_widget = waffle_calendar_input_widget,
    source_generator = function()
        return csource.calendar
    end,
}

-- local
waffle_calendar_view = wibox.widget{
    {
        {
            cwidget.cal_widget,
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
                                waffle_calendar_input_widget,
                                margins = dpi(2),
                                widget = wibox.container.margin,
                            },
                            draw_pickers = {
                                bg = prism.picker.concat{prism.picker.beautiful{"fg_normal"}, "20"},
                            },
                            widget = prism.container.background,
                        },
                        bottom = dpi(4),
                        draw_empty = false,
                        widget = fixed_margin,
                    },
                    {
                        waffle_calendar_bento,
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
}
local waffle_calendar_input = waffle_calendar_view:get_children_by_id("input_area")[1]
waffle_calendar_view = decorate_waffle(decorate_panel{widget = waffle_calendar_view})
function waffle_calendar_view:handle_key(mods, key, event)
    local captured
    if key == "[" or key == "]" or key == "\\" then
        for _, m in pairs(mods) do
            if m == "Mod4" then
                captured = true
                break
            end
        end
        if captured then
            if event == "release" then
                if key == "\\" then
                    notix.remove_unpinned()
                elseif key == "[" then
                    cwidget.cal_widget:move_date({month = -1})
                elseif key == "]" then
                    cwidget.cal_widget:move_date({month = 1})
                end
            end
            return true;
        end
    end
    return waffle_calendar_bento:handle_key(mods, key, event)
end
function waffle_calendar_view:handle_open(_screen, _is_new)
    waffle_calendar_bento:open()
    waffle_calendar_input.visible =
        not waffle:autohide()
end
function waffle_calendar_view:handle_close()
    waffle_calendar_bento:close()
    for s in screen do
        s.actions.set_clock_area_focus(false)
    end
end

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

--------------------------------------------------------------------------------
-- Client
--------------------------------------------------------------------------------

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
                outline_size = dpi(2),
                widget = outlined_textbox
            },
            draw_pickers = {
                fg = prism.picker.beautiful{"special_", prism.picker.highlighted_switcher},
            },
            widget = prism.container.background,
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

local waffle_client_name = wibox.widget{
    wrap = "word_char",
    align = "center",
    widget = wibox.widget.textbox,
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
local client_waffle_view = decorate_waffle{
    decorate_panel {
        widget = {
            {
                {
                    {
                        waffle_client_name,
                        margins = beautiful.sep_small_size,
                        draw_empty = false,
                        widget = fixed_margin,
                    },
                    draw_pickers = {
                        fg = prism.picker.beautiful{"fg_focus"},
                        bg = prism.picker.beautiful{"bg_focus"},
                    },
                    widget = prism.container.background,
                },
                width = dpi(64) * 3,
                widget = wibox.container.constraint,
            },
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
                               if client.valid then
                                   client.minimized = not client.minimized
                               end
                               if not alt or not client.valid then
                                   waffle:hide()
                               end
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
}

function client_waffle_view:handle_open(_screen, _is_new)
    if client_waffle_attached and not client_waffle_transient then
        awful.client.focus.history.disable_tracking()
    end
    client_waffle_transient = false
    client_close_count = 0
end
function client_waffle_view:handle_close()
    if client_waffle_attached and not client_waffle_transient then
        awful.client.focus.history.enable_tracking()
        local c = shared.waffle_selected_client
        client_waffle_attached = false
        if c and c.valid and c:isvisible() then
            gtimer.delayed_call(function () c:emit_signal("request::activate", "switch", {raise=true}) end)
        end
    end
    client_waffle_transient = false
    -- TODO check why I did this.
    -- if shared.waffle_selected_client and shared.waffle_selected_client.valid then
    --     shared.waffle_selected_client:emit_signal("property::name")
    -- end
    shared.waffle_selected_client = nil
end
local client_waffle_view_handle_key = aggregate_key_handlers{
    widget = client_waffle_view,
    key_filter = function (mod, key, event)
        for i = 1, #mod do
            if mod[i] == "Control" then return false end
        end
        return true
    end,
    handle_key_fallback = function (mod, key, event)
        if event ~= "press" then return end
        if key == "Return" then
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
function client_waffle_view:handle_key(mods, key, event)
    return client_waffle_view_handle_key(mods, key, event)
end

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
    waffle:show(client_waffle_view, args)
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

-- local
update_client_waffle_labels = function ()
    if not waffle:is_in_view(client_waffle_view) then
        return
    end

    if shared.waffle_selected_client == nil or not shared.waffle_selected_client.valid then
        return
    end

    local now_group = shared.waffle_selected_client.cgroup ~= nil
    if group_cache ~= now_group then
        group_cache = now_group
        group_label:set_markup(now_group and "GROUP" or "group")
    end

    local now_sticky = shared.waffle_selected_client.sticky
    if sticky_cache ~= now_sticky then
        sticky_cache = now_sticky
        sticky_label:set_markup(now_sticky and "STICKY" or "sticky")
    end

    local now_above = shared.waffle_selected_client.above
    if above_cache ~= now_above then
        above_cache = now_above
        above_label:set_markup(now_above and "ABOVE" or "above")
    end

    local now_float = shared.waffle_selected_client.floating
    if float_cache ~= now_float then
        float_cache = now_float
        float_label:set_markup(now_float and "FLOAT" or "float")
    end

    local now_max = shared.waffle_selected_client.maximized
    if max_cache ~= now_max then
        max_cache = now_max
        max_label:set_markup(now_max and "MAX" or "max")
    end

    local now_min = shared.waffle_selected_client.minimized
    if min_cache ~= now_min then
        min_cache = now_min
        min_label:set_markup(now_min and "MIN" or "min")
    end

    waffle_client_name.text = shared.waffle_selected_client.name or ""

    if client_close_count == 0 then
        close_label:set_markup("Close")
    else
        close_label:set_markup("Close "..tostring(client_close_count))
    end
end

capi.client.connect_signal("property::name", update_client_waffle_labels)
capi.client.connect_signal("property::cgroup", update_client_waffle_labels)
capi.client.connect_signal("property::sticky", update_client_waffle_labels)
capi.client.connect_signal("property::above", update_client_waffle_labels)
capi.client.connect_signal("property::floating", update_client_waffle_labels)
capi.client.connect_signal("property::maximized", update_client_waffle_labels)
capi.client.connect_signal("property::minimized", update_client_waffle_labels)

return nil

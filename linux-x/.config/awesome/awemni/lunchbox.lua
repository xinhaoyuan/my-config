-- Dashboard/launcher hybrid.
local prefix = (...):match("(.-)[^%.]+$")
local lister = require(prefix.."lister")
local scrlist = require(prefix.."scrlist")
local wibox = require("wibox")
local gio = require("lgi").Gio
local beautiful = require("beautiful")
local awful = require("awful")
local gears = require("gears")
local dpi = require("beautiful.xresources").apply_dpi
local fixed_align = require("fixed_align")
local ocontainer = require("onion.container")
local opicker = require("onion.picker")
local icons = require("icons")
local masked_imagebox = require("masked_imagebox")
local fixed_margin = require("fixed_margin")
local debug_container = require("debug_container")
local fallback = require("fallback")

local function prompter(args)
    local kg_run = awful.keygrabber.run
    local fake_grabber = {}
    local prompt_handler
    awful.keygrabber.run = function (cb)
        prompt_handler = cb
        return fake_grabber
    end
    awful.prompt.run(args)
    awful.keygrabber.run = kg_run
    return function (...)
        local kg_stop = awful.keygrabber.stop
        local stop_called
        awful.keygrabber.stop = function (grabber, ...)
            if grabber ~= fake_grabber then
                return kg_stop(grabber, ...)
            end
            stop_called = true
        end
        local ret = prompt_handler(...)
        awful.keygrabber.stop = kg_stop
        if stop_called then ret = false end
        return ret
    end
end

local lunchbox = {}

function lunchbox:new(args)
    local input_widget = wibox.widget{
        align = "center",
        widget = wibox.widget.textbox
    }
    local list_layout = wibox.widget{
        layout = scrlist.top,
    }

    local input_list_widget = wibox.widget{
        {
            {
                {
                    {
                        {
                            input_widget,
                            margins = dpi(2),
                            widget = wibox.container.margin,
                        },
                        bg = beautiful.fg_normal.."20",
                        widget = wibox.container.background,
                    },
                    {
                        list_layout,
                        top = 8,
                        draw_empty = false,
                        widget = fixed_margin,
                    },
                    layout = fixed_align.vertical
                },
                margins = dpi(4),
                widget = wibox.container.margin,
            },
            bg = beautiful.bg_normal,
            fg = beautiful.fg_normal,
            widget = wibox.container.background,
        },
        width = dpi(500),
        height = dpi(400),
        strategy = "exact",
        widget = wibox.container.constraint,
    }

    local lunchbox_lister = lister{
        scrlist = list_layout,
        fetch_size = args.fetch_size,
    }

    local function go_up()
        local f = lunchbox_lister.focus
        lunchbox_lister.focus = f and f - 1 or 1
        if lunchbox_lister.focus and list_layout.start_index and
            lunchbox_lister.focus < list_layout.start_index then
            list_layout.anchor_index = lunchbox_lister.focus
            list_layout.alignment = "start"
        end
    end

    local function go_down()
        local f = lunchbox_lister.focus
        lunchbox_lister.focus = f and f + 1 or 1
        if lunchbox_lister.focus and list_layout.end_index and
            lunchbox_lister.focus > list_layout.end_index then
            list_layout.anchor_index = lunchbox_lister.focus
            list_layout.alignment = "end"
        end
    end

    local function go_prior()
        if list_layout.start_index then
            lunchbox_lister.focus = list_layout.start_index - 1
            list_layout.anchor_index = lunchbox_lister.focus
            list_layout.alignment = "end"
        end
    end

    local function go_next()
        if list_layout.end_index then
            lunchbox_lister.focus = list_layout.end_index + 1
            list_layout.anchor_index = lunchbox_lister.focus
            list_layout.alignment = "start"
        end
    end

    local source_mode

    local prompt = prompter{
        textbox = input_widget,
        hooks = {
            {{}, "Escape", function (input)
                 source_mode = nil
                 lunchbox_lister.source = nil
                 lunchbox_lister.input = ""
                 if args.dashboard then
                     args.dashboard.widget.visible = true
                 end
                 return "", false
             end},
        },
        changed_callback = function (input)
            local previous_input = lunchbox_lister.input
            if lunchbox_lister.source == nil then
               lunchbox_lister.source = args.source_generator(source_mode)
            end
            lunchbox_lister.input = input
            if args.dashboard then
                args.dashboard.widget.visible = input == ""
            end
        end,
    }

    input_widget:connect_signal(
        "button::release", function (_self, _x, _y, b)
            if b == 3 then
                prompt({}, "Escape", "press")
            end
        end)

    list_layout:connect_signal(
        "button::press", function (_self, _x, _y, b)
            if b == 4 then
                go_prior()
            elseif b == 5 then
                go_next()
            end
        end)

    local dashboard_container = wibox.widget{
        args.dashboard.widget,
        widget = ocontainer,
    }
    local content_widget = wibox.widget{
        dashboard_container,
        input_list_widget,
        layout = fallback,
    }

    args.container:get_children_by_id("lunchbox_container")[1].widget = content_widget

    local should_swallow_next_release = true
    local pass_through_to_dashboard = false
    return {
        widget = args.container,
        set_source_mode = function (mode)
            source_mode = mode
            lunchbox_lister.source = args.source_generator(source_mode)
        end,
        on_open = function (...)
            should_swallow_next_release = true
            pass_through_to_dashboard = false
            dashboard_container.context_transformation = {
                inactive_hotkey = true
            }
            if args.on_open then args.on_open(...) end
        end,
        on_close = function (...)
            prompt({}, "Escape", "press")
            source_mode = nil
            if args.on_close then args.on_close(...) end
        end,
        key_handler = function (_self, mods, key, event)
            if args.dashboard and args.dashboard.widget.visible then
                if should_swallow_next_release then
                    should_swallow_next_release = false
                    if event == "release" then return true end
                end
                if key == "Escape" or key == "BackSpace" then return false end
                if key == "Alt_L" or key == "Alt_R" and not pass_through_to_dashboard then
                    if event == "press" then
                        dashboard_container.context_transformation = {
                            inactive_hotkey = false
                        }
                    else
                        dashboard_container.context_transformation = {
                            inactive_hotkey = true
                        }
                    end
                    return true
                end
                if key == " " then
                    if event == "release" then
                        pass_through_to_dashboard = not pass_through_to_dashboard
                        dashboard_container.context_transformation = {
                            inactive_hotkey = not pass_through_to_dashboard
                        }
                    end
                    return true
                else
                    local pass = #key > 1 or pass_through_to_dashboard
                    if not pass then
                        for i, m in ipairs(mods) do
                            if m == "Mod1" then pass = true; break end
                        end
                    end
                    if pass and args.dashboard.key_handler and
                        args.dashboard:key_handler(mods, key, event) then
                        return true
                    end
                end
            end
            should_swallow_next_release = true
            if event == "press" then
                if key == "Return" then
                    lunchbox_lister:execute()
                    return true
                elseif key == "Escape" then
                    prompt(mods, key, event)
                    return true
                elseif key == "Up" then
                    go_up()
                    return true
                elseif key == "Down" then
                    go_down()
                    return true
                elseif key == "Prior" then
                    go_prior()
                    return true
                elseif key == "Next" then
                    go_next()
                    return true
                end
            end
            local f = lunchbox_lister.focused_child
            if f and f.key_handler then return f:key_handler(mods, key, event) end
            prompt(mods, key, event)
            return true
        end
    }
end

return setmetatable(lunchbox, {__call = function (self, ...) return self:new(...) end})

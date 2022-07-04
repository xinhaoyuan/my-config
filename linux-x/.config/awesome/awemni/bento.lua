-- Launcher widget with optional cover.

local prefix = (...):match("(.-)[^%.]+$")
local lister = require(prefix.."lister")
local scrlist = require(prefix.."scrlist")
local wibox = require("wibox")
local awful = require("awful")

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
    local list_layout = wibox.widget{
        layout = scrlist.top,
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

    local cover = args.cover
    local input_widget = args.container:get_children_by_id("input_widget")[1]
    assert(input_widget)
    local function reset_input()
        lunchbox_lister.source = nil
        lunchbox_lister.input = ""
        if cover then
            cover.active = true
            cover.direct_hotkey = false
        end
    end
    local prompt = prompter{
        textbox = input_widget,
        hooks = {
            {{}, "Escape", function (input)
                    reset_input()
                    return "", false
            end},
            {{}, "BackSpace", function (input)
                    if input == "" then
                        reset_input()
                        return "", false
                    end
                    input = input:sub(1, -2)
                    lunchbox_lister.input = input
                    return input, false
            end}
        },
        changed_callback = function (input)
            if lunchbox_lister.source == nil then
                lunchbox_lister.source = args.source_generator()
            end
            lunchbox_lister.input = input
            cover.active = false
        end,
    }

    local list_container = args.container:get_children_by_id("list_container")[1]
    assert(list_container)
    list_container.widget = list_layout
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

    local should_swallow_next_release = true
    return {
        widget = args.container,
        reload_source = function ()
            lunchbox_lister.source = args.source_generator()
        end,
        on_open = function (...)
            should_swallow_next_release = true
            if cover and cover.on_open then cover.on_open(...) end
        end,
        on_close = function (...)
            prompt({}, "Escape", "press")
            if cover and cover.on_close then cover.on_close(...) end
        end,
        key_handler = function (_self, mods, key, event)
            if cover and cover.active then
                if should_swallow_next_release then
                    should_swallow_next_release = false
                    if event == "release" then return true end
                end
                if key == "Escape" or key == "BackSpace" then return false end
                if key == "Alt_L" or key == "Alt_R" and not cover.direct_hotkey then
                    if event == "press" then
                        cover.direct_hotkey = true
                    else
                        cover.direct_hotkey = false
                    end
                    return true
                end
                if key == " " then
                    if event == "release" then
                        cover.direct_hotkey = not cover.direct_hotkey
                    end
                    return true
                else
                    local pass = #key > 1 or cover.direct_hotkey
                    if not pass then
                        for i, m in ipairs(mods) do
                            if m == "Mod1" then pass = true; break end
                        end
                    end
                    if pass and cover.key_handler then
                        return cover:key_handler(mods, key, event)
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

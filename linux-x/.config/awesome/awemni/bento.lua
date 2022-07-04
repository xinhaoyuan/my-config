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
    local bento_lister = lister{
        scrlist = list_layout,
        fetch_size = args.fetch_size,
    }

    local function go_up()
        local f = bento_lister.focus
        bento_lister.focus = f and f - 1 or 1
        if bento_lister.focus and list_layout.start_index and
            bento_lister.focus < list_layout.start_index then
            list_layout.anchor_index = bento_lister.focus
            list_layout.alignment = "start"
        end
    end

    local function go_down()
        local f = bento_lister.focus
        bento_lister.focus = f and f + 1 or 1
        if bento_lister.focus and list_layout.end_index and
            bento_lister.focus > list_layout.end_index then
            list_layout.anchor_index = bento_lister.focus
            list_layout.alignment = "end"
        end
    end

    local function go_prior()
        if list_layout.start_index then
            bento_lister.focus = list_layout.start_index - 1
            list_layout.anchor_index = bento_lister.focus
            list_layout.alignment = "end"
        end
    end

    local function go_next()
        if list_layout.end_index then
            bento_lister.focus = list_layout.end_index + 1
            list_layout.anchor_index = bento_lister.focus
            list_layout.alignment = "start"
        end
    end

    local cover = args.cover
    local input_widget = args.container:get_children_by_id("input_widget")[1]
    assert(input_widget)
    local function reset_input()
        bento_lister.source = args.source_generator()
        bento_lister.input = ""
        if cover then
            cover.active = true
        end
    end
    local prompt = prompter{
        textbox = input_widget,
        hooks = {
            {{}, "Escape", function (input)
                 bento_lister.input = ""
                 return "", false
             end},
            {{}, "BackSpace", function (input)
                 if input == "" then
                     return "", false
                 end
                 input = input:sub(1, -2)
                 bento_lister.input = input
                 return input, false
             end}
        },
        changed_callback = function (input)
            bento_lister.input = input
            if #input > 0 then
                cover.active = false
            end
        end,
    }

    local should_swallow_next_release = true
    local function key_handler(mods, key, event)
        if (key == "Escape" or key == "BackSpace") and
            (bento_lister.input == nil or bento_lister.input == "") then
            if cover and not cover.active and cover.key_handler and event == "press" then
                reset_input()
                should_swallow_next_release = true
                return true
            elseif event == "press" then
                should_swallow_next_release = false
                return true
            elseif should_swallow_next_release then
                should_swallow_next_release = false
                return true
            else
                return false
            end
        end
        if cover and cover.active and cover.key_handler then
            if should_swallow_next_release then
                should_swallow_next_release = false
                if event == "release" then return true end
            end
            if cover.direct_hotkey then
                return cover:key_handler(mods, key, event)
            else
                if cover:key_handler(mods, key, event) then return true end
            end
        end
        should_swallow_next_release = true
        if event == "press" then
            if key == "Return" then
                bento_lister:execute()
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
        local f = bento_lister.focused_child
        if f and f.key_handler then return f:key_handler(mods, key, event) end
        prompt(mods, key, event)
        return true
    end

    local list_container = args.container:get_children_by_id("list_container")[1]
    assert(list_container)
    list_container.widget = list_layout
    input_widget:connect_signal(
        "button::release", function (_self, _x, _y, b)
            if b == 3 then
                key_handler({}, "Escape", "press")
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

    reset_input()
    return {
        widget = args.container,
        reload_source = function ()
            bento_lister.source = args.source_generator()
        end,
        on_open = function (_self, ...)
            bento_lister.source = args.source_generator()
            should_swallow_next_release = true
            if cover and cover.on_open then cover:on_open(...) end
        end,
        on_close = function (_self, ...)
            reset_input()
            prompt({}, "Escape", "press")
            if cover and cover.on_close then cover:on_close(...) end
        end,
        key_handler = function (_self, mods, key, event)
            return key_handler(mods, key, event)
        end
    }
end

return setmetatable(lunchbox, {__call = function (self, ...) return self:new(...) end})

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

local bento = {}

function bento:new(args)
    local list_layout = wibox.widget{
        layout = scrlist.top,
    }
    local bento_lister = lister(
        setmetatable(
            {scrlist = list_layout},
            {__index = args}))

    local function go_up()
        local f = bento_lister.focus
        bento_lister.focus = f and f - 1 or 1
    end

    local function go_down()
        local f = bento_lister.focus
        bento_lister.focus = f and f + 1 or 1
    end

    local function go_prior()
        if list_layout.start_index then
            list_layout.anchor_index = list_layout.start_index - 1
            bento_lister.focus = list_layout.anchor_index
            list_layout.alignment = "end"
        end
    end

    local function go_next()
        if list_layout.end_index then
            list_layout.anchor_index = list_layout.end_index + 1
            bento_lister.focus = list_layout.anchor_index
            list_layout.alignment = "start"
        end
    end

    local cover = args.cover
    local input_widget = args.container:get_children_by_id("input_widget")[1]
    assert(input_widget)
    local function reset()
        bento_lister.source = nil
        bento_lister.input = ""
        if cover then
            cover.active = true
            cover.direct_hotkey = false
        end
    end
    local prompt, key_handler
    prompt = prompter{
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
            if #input > 0 and cover then
                cover.active = false
            end
        end,
        done_callback = function ()
            key_handler({}, "Escape", "press")
        end,
    }
    function key_handler(mods, key, event)
        if (key == "Escape" or key == "BackSpace") and
            (bento_lister.input == nil or bento_lister.input == "") then
            if key == "BackSpace" and cover and not cover.active and cover.key_handler and event == "press" then
                reset()
                bento_lister.source = args.source_generator()
                return true
            else
                return false
            end
        end
        if cover and cover.active and cover.key_handler then
            if cover.direct_hotkey then
                return cover:key_handler(mods, key, event)
            else
                if cover:key_handler(mods, key, event) then return true end
            end
        end
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
        local f = bento_lister.focus and list_layout.children[bento_lister.focus]
        f = f and f.child
        if f and f.visible and f.key_handler then return f:key_handler(mods, key, event) end
        prompt(mods, key, event)
        return true
    end

    local list_container = args.container:get_children_by_id("list_container")[1]
    assert(list_container)
    list_container.widget = list_layout
    list_layout:connect_signal(
        "button::press", function (_self, _x, _y, b)
            if b == 4 then
                go_prior()
            elseif b == 5 then
                go_next()
            end
        end)

    reset()
    return {
        widget = args.container,
        reload_source = function ()
            bento_lister.source = args.source_generator()
        end,
        on_open = function (_self, ...)
            bento_lister.source = args.source_generator()
            if cover and cover.on_open then cover:on_open(...) end
        end,
        on_close = function (_self, ...)
            reset()
            prompt({}, "Escape", "press")
            if cover and cover.on_close then cover:on_close(...) end
        end,
        key_handler = function (_self, mods, key, event)
            return key_handler(mods, key, event)
        end
    }
end

return setmetatable(bento, {__call = function (self, ...) return self:new(...) end})
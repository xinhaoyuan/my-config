-- Listing widget associated with a prompt input box.

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

function bento.new(args)
    local list_layout = wibox.widget{
        layout = scrlist[args.list_direction or "top"],
    }

    local bento_lister = lister{
        scrlist = list_layout,
        placeholder_widget = args.placeholder_widget
    }

    local function go_up()
        local f = bento_lister.focus
        bento_lister.focus = f and f - 1 or list_layout.end_index
    end

    local function go_down()
        local f = bento_lister.focus
        bento_lister.focus = f and f + 1 or list_layout.start_index
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

    list_layout:connect_signal(
        "button::press", function (_self, _x, _y, b)
            if b == 4 then
                go_prior()
            elseif b == 5 then
                go_next()
            end
        end)

    local input_widget = args.input_widget
    assert(input_widget)
    local prompt, key_handler
    prompt = prompter{
        textbox = input_widget,
        hooks = {
            {{}, "Escape", function (_input)
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
        end,
        done_callback = function ()
            key_handler({}, "Escape", "press")
        end,
    }

    local function reset()
        bento_lister.source = nil
        bento_lister.input = ""
    end

    function list_layout:reload_source()
        bento_lister.source = args.source_generator()
    end

    function list_layout:open()
        bento_lister.source = args.source_generator()
    end

    function list_layout:close()
        reset()
        prompt({}, "Escape", "press")
    end

    function list_layout:handle_key(mods, key, event)
        if (key == "Escape" or key == "BackSpace") and
            (bento_lister.input == nil or bento_lister.input == "") then
            if event == "press" then
                reset()
            end
            return false
        elseif key == "Escape" then
            prompt(mods, key, event)
            return true
        elseif key == "Up" then
            if event == "press" then
                go_up()
            end
            return true
        elseif key == "Down" then
            if event == "press" then
                go_down()
            end
            return true
        elseif key == "Prior" then
            if event == "press" then
                go_prior()
            end
            return true
        elseif key == "Next" then
            if event == "press" then
                go_next()
            end
            return true
        end
        local f = bento_lister.focus and list_layout.children[bento_lister.focus]
        f = f and f.child
        if f and f.visible and f.handle_key and f:handle_key(mods, key, event) then
            return true
        end
        if key == "Return" then
            if event == "press" then bento_lister:execute() end
            return true
        end
        prompt(mods, key, event)
        return true
    end

    return list_layout
end

return setmetatable(bento, {__call = function (_self, ...) return bento.new(...) end})

local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local capi = {
    awesome = awesome,
}

local awful  = require("awful")
local be     = require("beautiful")
local gshape = require("gears.shape")
local wi     = require("wibox")
local beautiful = require("beautiful")
local dpi    = require("beautiful.xresources").apply_dpi
local unpack = unpack or table.unpack
local autolocker = require("autolocker")

local term_cmd
local browser_cmd
do
    local term_prog = "urxvt"
    if os.execute("command -v st") then
        term_prog = "st"
    elseif os.execute("command -v urxvt") then
        term_prog = "urxvt"
    else
        term_prog = "x-termninal-emulator"
        print("Warning: no suitable terminal program found. Fallback to x-terminal-emulator.")
    end
    local has_tabbed = false -- os.execute("command -v tabbed")

    if has_tabbed and term_prog == "st" then
        term_cmd = {"tabbed", "-kc", "-F", beautiful.fontname_normal .. ":size=10", "-M", beautiful.fg_normal, "-m", beautiful.bg_normal, "-T", beautiful.fg_focus, "-t", beautiful.bg_focus, "-r", "2", "--", "st", "-w", "--"}
    elseif has_tabbed and term_prog == "urxvt" then
        term_cmd = {"tabbed", "-kc", "-F", beautiful.fontname_normal .. ":size=10", "-M", beautiful.fg_normal, "-m", beautiful.bg_normal, "-T", beautiful.fg_focus, "-t", beautiful.bg_focus, "-r", "2", "--", "urxvt", "-embed", "--"}
    else
        term_cmd = {term_prog}
    end

    browser_cmd = {"firefox-esr"}
    -- if os.execute("command -v luakit") then
    --     browser_cmd = {"luakit"}
    -- else
    --     browser_cmd = {"firefox"}
    -- end
end

shared.action = {
    terminal = function (extra_cmd)
        local cmd = {unpack(term_cmd)}
        if type(extra_cmd) == "table" then
            table.insert(cmd, "-e")
            for i = 1, #extra_cmd do
                table.insert(cmd, extra_cmd[i])
            end
        end
        awful.spawn(cmd)
    end,
    terminal_session = function (args)
        args = args or {}
        local name = args.name or "default"
        local cmd = args.command or {}
        local real_cmd = {"tmux", "new", "-ADs", name}
        for _, c in ipairs(cmd) do
            real_cmd[#real_cmd + 1] = c
        end
        shared.action.terminal(real_cmd)
    end,
    web_browser = function (url)
        local cmd = browser_cmd
        if url then table.insert(cmd, url) end
        awful.spawn(cmd)
    end,
    file_manager = function (path)
        local cmd = {"pcmanfm"}
        if path then table.insert(cmd, path) end
        awful.spawn(cmd)
    end,
    launcher = function ()
        local cmd = {"rofi", "show",
                     "-normal-window",
                     "-combi-modi", "window,drun",
                     "-show", "combi",
                     "-modi", "combi,xmode:rofi_xmode.sh",
                     "-sidebar-mode",
                     "-font", beautiful.font,
                     "-dpi", tostring(dpi(96))}
        awful.spawn(cmd, false)
    end,
    calendar = function ()
         shared.action.terminal_session {
             name = "calendar",
             command = {"emacs", "-nw", os.getenv("HOME").."/org/TODO.org"}
         }
    end,
    app_finder = function ()
        awful.spawn("xfce4-appfinder")
    end,
    screen_locker = function ()
        awful.spawn({"i3lock", "-e", "-c", "404040"}, false)
    end,
    audio_setup = function (method, arg)
        if method == "mute-toggle" then
            awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
        elseif method == "volume-adjust" then
            if arg > 0 then
                awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +" .. tostring(arg) .. "%", false)
            else
                awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ " .. tostring(arg) .. "%", false)
            end
        end
    end,
    wallpaper_setup = function (restore)
        if restore then
            awful.spawn("nitrogen --restore", false)
        else
            awful.spawn("nitrogen", false)
        end
    end,
    music_app = function ()
        -- cantata does not respect startup on the activating existing window
        awful.spawn("cantata", false)
    end,
    reload_theme = function ()
        if beautiful.theme_path then
            beautiful.init(beautiful.theme_path.."/theme.lua")
            capi.awesome.emit_signal("wallpaper_changed")
            -- Workaround before making the taglist dynamic.
            for s in screen do
                s.widgets.tag_list:_do_taglist_update()
            end
        else
            print("Theme path not found - not reloading the theme.")
        end
    end
}

return nil

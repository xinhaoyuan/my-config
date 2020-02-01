local shared = require((...):match("(.-)[^%.]+$") .. "shared")

local awful  = require("awful")
local be     = require("beautiful")
local gshape = require("gears.shape")
local wi     = require("wibox")
local beautiful = require("beautiful")
local dpi    = require("beautiful.xresources").apply_dpi
local unpack = unpack or table.unpack
local autolocker = require("autolocker")

local term_cmd
do
    local term_prog = "urxvt"
    -- if os.execute("command -v xst") then
    --     term_prog = "xst"
    -- elseif os.execute("command -v urxvt") then
    --     term_prog = "urxvt"
    -- else
    --     term_prog = "x-termninal-emulator"
    --     print("Warning: no suitable terminal program found. Fallback to x-terminal-emulator.")
    -- end
    local has_tabbed = os.execute("command -v tabbed")

    if has_tabbed and term_prog == "xst" then
        term_cmd = {"tabbed", "-c", "-F", beautiful.fontname_normal .. ":size=10", "-M", beautiful.fg_normal, "-m", beautiful.bg_normal, "-T", beautiful.fg_focus, "-t", beautiful.bg_focus, "-r", "2", "--", "xst", "-w", "--"}
    elseif has_tabbed and term_prog == "urxvt" then
        term_cmd = {"tabbed", "-c", "-F", beautiful.fontname_normal .. ":size=10", "-M", beautiful.fg_normal, "-m", beautiful.bg_normal, "-T", beautiful.fg_focus, "-t", beautiful.bg_focus, "-r", "2", "--", "urxvt", "-embed", "--"}
    else
        term_cmd = {term_prog}
    end
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
        local real_cmd = {"tmux", "new", "-As", name}
        for _, c in ipairs(cmd) do
            real_cmd[#real_cmd + 1] = c
        end
        shared.action.terminal(real_cmd)
    end,
    web_browser = function (url)
        local cmd = {"x-www-browser"}
        if url then table.insert(cmd, url) end
        awful.spawn(cmd)
    end,
    file_manager = function (path)
        local cmd = {"thunar"}
        if path then table.insert(cmd, path) end
        awful.spawn(cmd)
    end,
    launcher = function ()
        local cmd = {"rofi", "show",
                     "-normal-window",
                     "-combi-modi", "window,drun",
                     "-show-icons",
                     "-show", "combi",
                     "-modi", "combi",
                     "-font", beautiful.font}
        awful.spawn(cmd, false)
    end,
    calendar = function ()
         shared.action.terminal_session {
             name = "org-mode",
             command = {"emacs", "-nw", "-e", "org-agenda-list"}
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
}

return nil

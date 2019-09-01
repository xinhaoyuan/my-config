local awful  = require("awful")
local be     = require("beautiful")
local gshape = require("gears.shape")
local wi     = require("wibox")
local dpi    = require("beautiful.xresources").apply_dpi

be.init("~/.config/awesome/theme.lua")

local config = {
   tag_filter = function (name)
      return name ~= "STICKY"
   end,
   action_terminal = function (extra_cmd)
      local cmd = {"urxvt"}
      if type(extra_cmd) == "table" then
         table.insert(cmd, "-e")
         for i = 1, #extra_cmd do
            table.insert(cmd, extra_cmd[i])
         end
      end
      awful.spawn(cmd)
   end,
   action_web_browser = function (url)
      local cmd = {"x-www-browser"}
      if url then table.insert(cmd, url) end
      awful.spawn(cmd)
   end,
   action_file_manager = function (path)
      local cmd = {"thunar"}
      if path then table.insert(cmd, path) end
      awful.spawn(cmd)
   end,
   action_launcher = function ()
      local cmd = {"rofi", "show",
                   "-combi-modi", "window,drun",
                   "-show", "combi",
                   "-modi", "combi",
                   "-font", be.mono_font or be.font}
      awful.spawn(cmd)
   end,
   action_app_finder = function ()
      awful.spawn("xfce4-appfinder")
   end,
   action_screen_locker = function ()
      awful.spawn({"i3lock", "-e", "-c", "404040"}, false)
   end,
   action_audio_setup = function (method, arg)
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
}

return config

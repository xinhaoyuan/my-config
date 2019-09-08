local awful  = require("awful")
local be     = require("beautiful")
local gshape = require("gears.shape")
local wi     = require("wibox")
local dpi    = require("beautiful.xresources").apply_dpi

local action = {
   terminal = function (extra_cmd)
      local cmd = {"urxvt"}
      if type(extra_cmd) == "table" then
         table.insert(cmd, "-e")
         for i = 1, #extra_cmd do
            table.insert(cmd, extra_cmd[i])
         end
      end
      awful.spawn(cmd)
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
                   "-combi-modi", "window,drun",
                   "-show", "combi",
                   "-modi", "combi",
                   "-font", be.mono_font or be.font}
      awful.spawn(cmd)
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
}

return action

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
   cmd_terminal = "urxvt",
   gen_terminal_with_cmd = function (cmd)
      local ret = {"urxvt", "-e"}
      if type(cmd) == "table" then
         for i = 1, #cmd do
            table.insert(ret, cmd[i])
         end
      end
      return ret
   end,
   cmd_web_browser = "x-www-browser",
   cmd_file_manager = "thunar",
}

return config

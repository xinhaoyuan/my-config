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
   cmd_file_manager = "thunar",
}

return config

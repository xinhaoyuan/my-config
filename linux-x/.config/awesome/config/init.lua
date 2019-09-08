local local_path = debug.getinfo(1, "S").source:match("^@(.-)[^/]+$")

local beautiful = require("beautiful")
beautiful.init(local_path .. "/theme.lua")

local mod = {
   action = require(... .. ".action"),
   screen = require(... .. ".screen"),
   client = require(... .. ".client"),
   waffle = require(... .. ".waffle"),
}

return mod

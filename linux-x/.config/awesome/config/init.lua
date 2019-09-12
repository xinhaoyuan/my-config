local local_path = debug.getinfo(1, "S").source:match("^@(.-)[^/]+$")

local beautiful = require("beautiful")
beautiful.init(local_path .. "/theme.lua")

require(... .. ".action")
require(... .. ".screen")
require(... .. ".client")
require(... .. ".waffle")

return require(... .. ".shared")

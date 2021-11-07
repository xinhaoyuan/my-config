-- Override the new tab settings.
local window = require("window")
local settings = require("settings")

settings.override_setting("window.home_page", "luakit://bookmarks/")
settings.override_setting("window.new_tab_page", "luakit://bookmarks/")

local modes = require("modes")
local window = require("window")
local vertical_tab = require("vertical_tab")
local tablist = require("lousy.widget.tablist")
local settings = require("settings")

modes.add_binds(
    "normal", {
        { "<Control-c>", "Copy selected text.", function ()
              luakit.selection.clipboard = luakit.selection.primary
          end},
    })

tablist.min_width = 0

settings.set_setting("tablist.visibility", "always")
settings.set_setting("webview.zoom_level", 150)

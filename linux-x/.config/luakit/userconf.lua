local modes = require("modes")
local window = require("window")
local vertical_tab = require("vertical_tab")
local tablist = require("lousy.widget.tablist")
local settings = require("settings")
local webview = require("webview")

modes.add_binds(
    "normal", {
        { "<Control-c>", "Copy selected text.", function ()
              luakit.selection.clipboard = luakit.selection.primary
          end},
    })

tablist.min_width = 0

local global_stylesheet = stylesheet{
    source = [===[
@font-face {
  font-family: "ui-monospace";
  src: local("monospace");
}
]===] }

webview.add_signal(
    "init", function (view)
        view:add_signal(
            "stylesheet", function (v)
                v.stylesheets[global_stylesheet] = true;
            end)
    end)

settings.set_setting("tablist.visibility", "always")
settings.set_setting("webview.zoom_level", 150)

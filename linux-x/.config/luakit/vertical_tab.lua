--- UI mod: vertical tabs.
--
-- This module moves the tab bar to the side.
--
-- @module vertical_tabs
-- @copyright 2017 Aidan Holm <aidanholm@gmail.com>

local _M = {}

local window = require("window")
local lousy = require("lousy")
local modes = require("modes")
local settings = require("settings")
local button = require("button")

local appear_cb = setmetatable({}, { __mode = "k" })

window.add_signal("build", function (w)
    -- Replace the existing tablist with a vertical one
    w.tablist:destroy()
    w.tablist = lousy.widget.tablist(w.tabs, "vertical")
    local tablist_box = widget{ type = "eventbox" }
    local tablist_overlay = widget{ type = "overlay" }
    local sizing_widget = widget{ type = "eventbox" }
    tablist_box.child = tablist_overlay
    tablist_overlay.child = sizing_widget

    local control_widget_container = widget{ type = "hbox" }
    control_widget_container.min_size = { h = 40, w = settings.get_setting("vertical_tabs.sidebar_width") }
    local new_tab_button = button{
        icon_text = "<big>+</big>",
        icon_bg = "#0ad",
        text = "New tab",
        on_click = function ()
            w:new_tab(settings.get_setting("window.new_tab_page"))
        end
    }
    control_widget_container:pack(new_tab_button.widget, { expand = true, fill = true })

    local tablist_container = widget{ type = "vbox" }
    tablist_container:pack(control_widget_container)
    tablist_container:pack(w.tablist.widget, { expand = true, fill = true })
    tablist_overlay:pack(tablist_container, { halign = "fill", valign = "fill"})

    local left = settings.get_setting("vertical_tabs.side") == "left"
    local padding = settings.get_setting("vertical_tabs.sidebar_padding")
    sizing_widget.min_size = { w = padding }
    w.tabs[left and "margin_left" or "margin_right"] = padding

    w.menu_tabs:pack(tablist_box, { halign = left and "start" or "end", valign = "fill" })
    w.menu_tabs:reorder(tablist_box, 0)
    local shrink_timer = timer{ interval = 500 }
    shrink_timer:add_signal(
        "timeout", function ()
            sizing_widget.min_size = { w = settings.get_setting("vertical_tabs.sidebar_padding") }
            shrink_timer:stop()
        end)
    tablist_box:add_signal(
        "mouse-enter", function (t)
            if shrink_timer.started then
                shrink_timer:stop()
            end
            sizing_widget.min_size = { w = settings.get_setting("vertical_tabs.sidebar_width") }
        end)
    tablist_box:add_signal(
        "mouse-leave", function (t)
            if shrink_timer.started then
                shrink_timer:stop()
            end
            shrink_timer:start()
        end)
end)

settings.register_settings({
    ["vertical_tabs.sidebar_padding"] = {
        type = "number", min = 0,
        -- 32 = size of favicons
        -- 5  = margin_left of tab entry
        -- 3  < margin_left of tab label
        default = 32 + 5 + 3,
    },
    ["vertical_tabs.sidebar_width"] = {
        type = "number", min = 0,
        default = 400,
    },
    ["vertical_tabs.side"] = {
        type = "enum",
        options = {
            ["left"] = { desc = "Left side of the screen.", label = "Left", },
            ["right"] = { desc = "Right side of the screen.", label = "Right", },
        },
        default = "left",
        desc = "The side of the window that the vertical tabs sidebar should be shown on.",
    },
})

settings.add_signal("setting-changed", function (e)
    if e.key == "vertical_tabs.side" then
        for _, w in pairs(window.bywidget) do
            local paned = w.menu_tabs.child
            assert(paned.type == "hpaned")

            local l, r, width, pos = paned.left, paned.right, paned.width, paned.position
            paned:remove(l)
            paned:remove(r)
            paned:pack1(r)
            paned:pack2(l)
            -- don't flip position if the position is unset
            if not appear_cb[paned] then
                paned.position = width - pos
            end
        end
    end
end)

settings.migrate_global("vertical_tabs.sidebar_width", "vertical_tab_width")

modes.add_binds(
    "normal", {
        { "`", "Toggle the vertical bar expansion.", function (w)
              local width = settings.get_setting("vertical_tabs.sidebar_width")
              local padding = settings.get_setting("vertical_tabs.sidebar_padding")
              if w.tablist_sizing_widget.min_size.width == width then
                  w.tablist_sizing_widget.min_size = { w = padding }
              else
                  w.tablist_sizing_widget.min_size = { w = width }
              end
          end, {} }
    })

return _M

-- vim: et:sw=4:ts=8:sts=4:tw=80

-- Based from awful/widget/layoutbox.lua

local capi = { screen = screen, tag = tag }
local layout = require("awful.layout")
local tooltip = require("awful.tooltip")
local beautiful = require("beautiful")
local wibox = require("wibox")
local surface = require("gears.surface")
local gdebug = require("gears.debug")
local gcolor = require("gears.color")
local gtable = require("gears.table")

local function get_screen(s)
    return s and capi.screen[s]
end

local layoutbox = { mt = {} }

local boxes = nil

local function update(w, screen)
    screen = get_screen(screen)
    local name = layout.getname(layout.get(screen))
    w._layoutbox_tooltip:set_text(name or "[no name]")

    w.bg_container:set_bg(w.focus and beautiful.bg_focus or beautiful.bg_normal)
    w.bg_container:set_fg(w.focus and beautiful.fg_focus or beautiful.fg_normal)
    if beautiful["layout_" .. name] ~= nil and beautiful["layout_" .. name .. "_focus"] == nil then
        beautiful["layout_" .. name .. "_focus"] = gcolor.recolor_image(beautiful["layout_" .. name], beautiful.fg_focus)
    end
    local img = surface.load_silently(beautiful[w.focus and "layout_" .. name .. "_focus" or "layout_" .. name ], false)
    w.imagebox.image = img
    w.textbox.text   = img and "" or name
end

local function update_from_tag(t)
    local screen = get_screen(t.screen)
    local w = boxes[screen]
    if w then
        update(w, screen)
    end
end

--- Create a layoutbox widget. It draws a picture with the current layout
-- symbol of the current tag.
-- @tparam table args The arguments.
-- @tparam screen args.screen The screen number that the layout will be represented for.
-- @tparam table args.buttons The `awful.button`s for this layoutbox.
-- @return The layoutbox.
function layoutbox.new(args)
    args = args or {}
    local screen = nil

    if type(args) == "number" or type(args) == "screen" or args.fake_remove then
        screen, args = args, {}

        gdebug.deprecate(
            "Use awful.widget.layoutbox{screen=s} instead of awful.widget.layoutbox(screen)",
            {deprecated_in=5}
        )
    end

    assert(type(args) == "table")

    screen = get_screen(screen or 1)

    -- Do we already have the update callbacks registered?
    if boxes == nil then
        boxes = setmetatable({}, { __mode = "kv" })
        capi.tag.connect_signal("property::selected", update_from_tag)
        capi.tag.connect_signal("property::layout", update_from_tag)
        capi.tag.connect_signal("property::screen", function()
            for s, w in pairs(boxes) do
                if s.valid then
                    update(w, s)
                end
            end
        end)
        layoutbox.boxes = boxes
    end

    -- Do we already have a layoutbox for this screen?
    local w = boxes[screen]
    if not w then
        w = wibox.widget {
            {
                {
                    id     = "imagebox",
                    widget = wibox.widget.imagebox
                },
                {
                    id     = "textbox",
                    widget = wibox.widget.textbox
                },
                layout = wibox.layout.fixed.horizontal
            },
            id = "bg_container",
            widget = wibox.container.background
        }

        w.imagebox = w:get_children_by_id("imagebox")[1]
        w.textbox = w:get_children_by_id("textbox")[1]
        w.bg_container = w:get_children_by_id("bg_container")[1]
        w._layoutbox_tooltip = tooltip {objects = {w}, delay_show = 1}
        w:connect_signal("update", function () update(w, screen) end) 

        -- Apply the buttons, visible, forced_width and so on
        gtable.crush(w, args)

        update(w, screen)
        boxes[screen] = w
    end

    return w
end

function layoutbox.mt:__call(...)
    return layoutbox.new(...)
end

--@DOC_widget_COMMON@

--@DOC_object_COMMON@

return setmetatable(layoutbox, layoutbox.mt)

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

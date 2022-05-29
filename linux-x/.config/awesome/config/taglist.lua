local shared = require((...):match("(.-)[^%.]+$") .. "shared")

local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local module = {}

function module.switch_or_restore(tag)
    local screen = tag.screen
    if #screen.selected_tags == 1 and screen.selected_tag == tag then
        awful.tag.history.restore(screen)
    else
        tag:view_only()
    end
end

local my_tag_list_buttons = awful.util.table.join(
    awful.button({ }, 1, module.switch_or_restore),
    awful.button({ "Mod4" }, 1, awful.client.movetotag),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ "Mod4" }, 3, awful.client.toggletag),
    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

local size_index = shared.size_index
local dual_size_index = shared.dual_size_index
local top_index = shared.top_index
local bottom_index = shared.bottom_index
local left_index = shared.left_index
local right_index = shared.right_index
local direction_index = shared.direction_index
local dual_direction_index = shared.dual_direction_index
local gravity_index = shared.gravity_index

function module.create(screen)
    return awful.widget.taglist{
        screen = screen,
        filter = function (t) return true end,
        buttons = my_tag_list_buttons,
        -- layout = wibox.layout.fixed[direction_index[shared.vars.bar_position]],
        layout = wibox.layout.grid[direction_index[shared.vars.bar_position]](beautiful.bar_rows),
        style = {
            font = "DejaVu Sans 10",
        },
        widget_template = {
            {
                {
                    id = "text_role",
                    widget = wibox.widget.textbox,
                },
                halign = "center",
                valign = "center",
                forced_width = beautiful.bar_height / beautiful.bar_rows,
                forced_height = beautiful.bar_height / beautiful.bar_rows,
                widget = wibox.container.place
            },
            id = "background_role",
            widget = wibox.container.background,
        }
    }
end

return module

-- Methods exposed externally and also shared across sub-modules.
-- The default config variables are defined here.

local hotpot = require("hotpot")
local gobject = require("gears.object")
local gtable = require("gears.table")

local shared = {
    vars = gtable.crush(
        gobject{enable_properties = true, enable_auto_signals = true},
        {
            enable_titlebar = true,
            -- "top"/"bottom"/"left"/"right"
            -- titlebar_position = "top",
            hide_clients_with_titlebars = true,
            floating_by_default = false,
            bar_position = "bottom",
            show_notes = true,
        }
    ),
    on_start_functions = {},
    -- Bottom is the baseline
    index_transform = {
        span_width = {
            ["top"] = "height",
            ["bottom"] = "height",
            ["left"] = "width",
            ["right"] = "width",
        },
        span_len = {
            ["top"] = "width",
            ["bottom"] = "width",
            ["left"] = "height",
            ["right"] = "height",
        },
        span_dir = {
            ["top"] = "horizontal",
            ["bottom"] = "horizontal",
            ["left"] = "vertical",
            ["right"] = "vertical",
        },
        north = {
            ["bottom"] = "top",
            ["top"] = "bottom",
            ["left"] = "right",
            ["right"] = "left",
        },
        south = {
            ["bottom"] = "bottom",
            ["top"] = "top",
            ["left"] = "left",
            ["right"] = "right",
        },
        west = {
            ["bottom"] = "left",
            ["top"] = "right",
            ["left"] = "top",
            ["right"] = "bottom",
        },
        east = {
            ["bottom"] = "right",
            ["top"] = "left",
            ["left"] = "bottom",
            ["right"] = "top",
        },
        dual = {
            ["bottom"] = "top",
            ["top"] = "bottom",
            ["left"] = "right",
            ["right"] = "left",
            ["horizontal"] = "vertical",
            ["vertical"] = "horizontal",
        },
    },
    -- To be deprecated
    size_index = {
        ["top"] = "height",
        ["bottom"] = "height",
        ["left"] = "width",
        ["right"] = "width",
    },
    dual_size_index = {
        ["top"] = "width",
        ["bottom"] = "width",
        ["left"] = "height",
        ["right"] = "height",
    },
    top_index = {
        ["bottom"] = "top",
        ["top"] = "bottom",
        ["left"] = "right",
        ["right"] = "left",
    },
    bottom_index = {
        ["bottom"] = "bottom",
        ["top"] = "top",
        ["left"] = "left",
        ["right"] = "right",
    },
    left_index = {
        ["bottom"] = "left",
        ["top"] = "right",
        ["left"] = "top",
        ["right"] = "bottom",
    },
    right_index = {
        ["bottom"] = "right",
        ["top"] = "left",
        ["left"] = "bottom",
        ["right"] = "top",
    },
    direction_index = {
        ["top"] = "horizontal",
        ["bottom"] = "horizontal",
        ["left"] = "vertical",
        ["right"] = "vertical",
    },
    dual_direction_index = {
        ["top"] = "vertical",
        ["bottom"] = "vertical",
        ["left"] = "horizontal",
        ["right"] = "horizontal",
    },
    gravity_index = {
        ["top"] = "northwest",
        ["bottom"] = "southwest",
        ["left"] = "northwest",
        ["right"] = "northeast",
    }
}

hotpot.on_ready(function ()
        for _, f in ipairs(shared.on_start_functions) do
            f()
        end
end)

return shared

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
    tasklist_order_function = function (clients)
        local ticket = {}
        for _, c in ipairs(clients) do
            if c.cgroup then
                local old_ticket = ticket[c.cgroup]
                ticket[c.cgroup] = (old_ticket == nil) and c.manage_ticket or math.min(old_ticket, c.manage_ticket)
            end
        end
        table.sort(clients,
                   function (a, b)
                       -- -- Minimized windows appear at last
                       -- if a.minimized ~= b.minimized then return b.minimized else return a.window < b.window end
                       local a_ticket = a.cgroup and ticket[a.cgroup] or a.manage_ticket
                       local b_ticket = b.cgroup and ticket[b.cgroup] or b.manage_ticket
                       if a_ticket == b_ticket then
                           return a.manage_ticket < b.manage_ticket
                       else
                           return a_ticket < b_ticket
                       end
                   end
                  )
        return clients
    end,
    on_start_functions = {},
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

local capi = {
    awesome = awesome,
    client = client,
}

local awful = require("awful")

local manage_ticket_max = 0
local module = {}
function module.get_new_ticket()
    manage_ticket_max = manage_ticket_max + 1
    return manage_ticket_max
end

awful.client.property.persist("manage_ticket", "number")
capi.client.connect_signal(
    "manage",
    function (c)
        if c.manage_ticket == nil then
            c.manage_ticket = module.get_new_ticket()
        end

        if c.manage_ticket > manage_ticket_max then
            manage_ticket_max = c.manage_ticket
        end
    end)

return module

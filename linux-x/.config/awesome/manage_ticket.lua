local capi = {
   awesome = awesome,
   client = client,
}

local awful = require("awful")

awful.client.property.persist("manage_ticket", "number")

local manage_ticket_max = 0
capi.client.connect_signal(
   "manage",
   function (c)
      if c.manage_ticket == nil then
         c.manage_ticket = manage_ticket_max + 1
      end

      if c.manage_ticket > manage_ticket_max then
         manage_ticket_max = c.manage_ticket
      end
end)

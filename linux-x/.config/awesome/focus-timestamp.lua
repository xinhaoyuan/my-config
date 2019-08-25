local awful = require("awful")
local fts = {}
local focus_timestamp = 0
local function update_focus_timestamp(c)
   if c == nil then return end
   if c.focus_timestamp ~= nil and
      c.focus_timestamp > focus_timestamp
   then
      focus_timestamp = c.focus_timestamp
   end
   focus_timestamp = focus_timestamp + 1
   c.focus_timestamp = focus_timestamp
end

client.connect_signal(
   "focus",
   function (c)
      if awful.client.focus.history.is_enabled() then
         update_focus_timestamp(c)
      end
   end
)

client.connect_signal(
   "manage",
   function (c)
      if c.focus_timestamp == nil then
         c.focus_timestamp = 0
      end
   end
)

fts.update = update_focus_timestamp

function fts.get(c)
   if c.focus_timestamp == nil then
      return -1
   else
      return c.focus_timestamp
   end
end

return fts

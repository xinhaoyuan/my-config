local awful = require("awful")
local module = {
   update_lock = 0,
}

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
      if module.update_lock > 0 then
         c.focus_timestamp_touched = true
      else
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

function module.lock()
   if module.update_lock == 0 then
      awful.client.focus.history.disable_tracking()
   end
   module.update_lock = module.update_lock + 1
end

function module.unlock()
   if module.update_lock > 0 then
      module.update_lock = module.update_lock - 1
      if module.update_lock == 0 then
         awful.client.focus.history.enable_tracking()
      end
   end
end

function module.get(c)
   if c.focus_timestamp == nil then
      return -1
   else
      return c.focus_timestamp
   end
end

module.update = update_focus_timestamp

return module

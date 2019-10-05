local spawn = require("awful.spawn")
local gtimer = require("gears.timer")
local gobject = require("gears.object")
local mod = {}

function mod.create(command, timeout)
   command = command or {"fortune", "-s"}
   timeout = timeout or 120

   local obj = gobject {}
   local t
   local obj_ptr = {obj}
   setmetatable(obj_ptr, {__mode="v"})

   t = gtimer { timeout = timeout }
   t:connect_signal(
      "timeout", function()
         t:stop()
         spawn.easy_async(
            command,
            function(stdout, stderr, exitreason, exitcode)
               if obj_ptr[1] then
                  if exitcode == 0 then
                     obj_ptr[1].fortune = stdout:gsub("\n", " "):gsub("%s+", " "):match("^%s*(.-)%s*$")
                     obj_ptr[1]:emit_signal("property::fortune", obj_ptr[1])
                  end
                  t:again()
               end
            end
         )
   end)
   t:start()
   t:emit_signal("timeout")
   return obj
end

return mod

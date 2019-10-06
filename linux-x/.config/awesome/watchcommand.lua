local spawn = require("awful.spawn")
local gtimer = require("gears.timer")
local gobject = require("gears.object")
local mod = {}

function mod.create(command, timeout)
   assert(command)
   timeout = timeout or 1

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
                     obj_ptr[1].output = stdout
                  else
                     obj_ptr[1].output = nil
                  end
                  obj_ptr[1]:emit_signal("property::output", obj_ptr[1])
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

local icons = {}
local local_path = debug.getinfo(1, "S").source:match("^@(.-)[^/]+$")

setmetatable(
    icons,
    {
        __index = function (self, name)
            if rawget(self, name) then
                return rawget(self, name)
            else
                return local_path..name..".svg"
            end
        end
    })
icons.calendar_todo = local_path .. "calendar-todo.svg"
icons.battery_full = local_path .. "battery-full.svg"
icons.battery_half = local_path .. "battery-half.svg"

return icons

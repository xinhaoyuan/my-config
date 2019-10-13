local mod = {}
local gcolor = require("gears.color")

function mod.string_to_rgba(s)
   return table.pack(gcolor.parse_color(s))
end

function mod.rgba_to_string(comp)
   local ret = "#"
   for i = 1, 4 do
      ret = ret .. string.format("%02x", math.min(255, math.floor(comp[i] * 256)))
   end
   return ret
end

function mod.blend(c1, w1, c2, w2)
   c1 = type(c1) == "string" and mod.string_to_rgba(c1) or c1
   c2 = type(c2) == "string" and mod.string_to_rgba(c2) or c2
   local ret = {}
   for i = 1, 4 do
      ret[i] = (c1[i] * w1 + c2[i] * w2) / (w1 + w2)
   end
   return ret
end
      
return mod

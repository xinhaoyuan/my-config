local aw = require("awful")
local ti = require("gears.timer")

local mod = {
   conky = nil
}

function mod.get_conky(default)
   if mod.conky and mod.conky.valid then
      return mod.conky
   end

   mod.conky = aw.client.iterate(function(c) return c.class == "Conky" end)()
   return mod.conky or default
end

function mod.raise_conky()
   mod.get_conky({}).ontop = true
end

function mod.lower_conky()
   mod.get_conky({}).ontop = false
end

function mod.lower_conky_delayed()
   ti.delayed_call(mod.lower_conky)
end

function mod.toggle_conky()
   local conky = mod.get_conky({})
   conky.ontop = not conky.ontop
end

return mod

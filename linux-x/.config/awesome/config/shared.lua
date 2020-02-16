-- Methods exposed externally and also shared across sub-modules.
-- The default config variables are defined here.

local gtimer = require("gears.timer")

local shared = {
   var = {
      enable_titlebar = true,
      -- "top"/"bottom"/"left"/"right"
      -- titlebar_position = "top",
      hide_clients_with_titlebars = true,
      floating_by_default = true,
      bar_position = "bottom",
   },
   on_start_functions = {},
}

gtimer.delayed_call(function ()
        for _, f in ipairs(shared.on_start_functions) do
            f()
        end
end)

return shared

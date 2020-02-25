-- Methods exposed externally and also shared across sub-modules.
-- The default config variables are defined here.

local hotpot = require("hotpot")

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

hotpot.config.on_ready(function ()
        for _, f in ipairs(shared.on_start_functions) do
            f()
        end
end)

return shared

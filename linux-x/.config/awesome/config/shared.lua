-- Methods exposed externally and also shared across sub-modules.
-- The default config variables are defined here.

local shared = {
   var = {
      enable_titlebar = false,
      -- "top"/"bottom"/"left"/"right"
      titlebar_position = "bottom",
      hide_clients_with_titlebars = true,
      floating_by_default = true,
      bar_position = "bottom",
   },
   on_start_functions = {},
}

function shared.start()
   for _, f in ipairs(shared.on_start_functions) do
      f()
   end
end

return shared

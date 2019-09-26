-- Methods will be defined in corresponding modules.
-- The default config variables are defined here.

local shared = {
   var = {
      enable_titlebar = false,
      floating_by_default = false,
      titlebar_position = "bottom",
   },
   on_start_functions = {},
}

function shared.start()
   for _, f in ipairs(shared.on_start_functions) do
      f()
   end
end

return shared

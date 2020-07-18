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
   size_index = {
       ["top"] = "height",
       ["bottom"] = "height",
       ["left"] = "width",
       ["right"] = "width",
   },
   dual_size_index = {
       ["top"] = "width",
       ["bottom"] = "width",
       ["left"] = "height",
       ["right"] = "height",
   },
   top_index = {
       ["bottom"] = "top",
       ["top"] = "bottom",
       ["left"] = "right",
       ["right"] = "left",
   },
   bottom_index = {
       ["bottom"] = "bottom",
       ["top"] = "top",
       ["left"] = "left",
       ["right"] = "right",
   },
   left_index = {
       ["bottom"] = "left",
       ["top"] = "right",
       ["left"] = "top",
       ["right"] = "bottom",
   },
   right_index = {
       ["bottom"] = "right",
       ["top"] = "left",
       ["left"] = "bottom",
       ["right"] = "top",
   },
   direction_index = {
       ["top"] = "horizontal",
       ["bottom"] = "horizontal",
       ["left"] = "vertical",
       ["right"] = "vertical",
   },
   dual_direction_index = {
       ["top"] = "vertical",
       ["bottom"] = "vertical",
       ["left"] = "horizontal",
       ["right"] = "horizontal",
   },
   gravity_index = {
       ["top"] = "northwest",
       ["bottom"] = "southwest",
       ["left"] = "northwest",
       ["right"] = "northeast",
   }
}

hotpot.on_ready(function ()
        for _, f in ipairs(shared.on_start_functions) do
            f()
        end
end)

return shared

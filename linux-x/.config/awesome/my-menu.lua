local awful = require("awful")
local beautiful = require("beautiful")
local freedesktop = require("freedesktop")

local menu_screen_layouts = {}

local files = io.popen('find $HOME/.screenlayout/ -maxdepth 1 -name "*.sh"'):lines()
for file in files do
   local layout_name = file:match(".-([^/]+).sh$")
   if #layout_name > 0 then
      local file_to_execute = file
      table.insert(menu_screen_layouts,
                   { layout_name,
                     function ()
                        awful.spawn(file_to_execute, false)
                     end
      })
   end
end

return freedesktop.menu.build({
      before = {
         { "awesome",
           {
              { "restart", awesome.restart },
              { "quit", awesome.quit },
              { "shutdown", function () os.execute("systemctl poweroff") end },
           },
           beautiful.awesome_icon
         },
         {
            "screen layouts",
            menu_screen_layouts,
         },
      },
      after = { },
})

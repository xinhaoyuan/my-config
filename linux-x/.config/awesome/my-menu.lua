local beautiful = require("beautiful")
local freedesktop = require("freedesktop")

return freedesktop.menu.build({
      before = { { "awesome",
                   {
                      { "restart", awesome.restart },
                      { "quit", awesome.quit },
                      { "shutdown", function () os.execute("systemctl poweroff") end },
                   },
                   beautiful.awesome_icon } },
      after = { },
})

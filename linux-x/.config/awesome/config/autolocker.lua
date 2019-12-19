local shared = require((...):match("(.-)[^%.]+$") .. "shared")
local autolocker = require("autolocker")
local spawn = require("awful.spawn")
local gtimer = require("gears.timer")

autolocker.ms_before_lock = 1000 * 60 * 5 -- 5 min
autolocker:connect_signal("trylock",
                          function ()
                              spawn({"xidletimer", "reset"})
                              shared.action.screen_locker()
                          end
)

local function start_autolocker_timer()
    gtimer.start_new(
        5, function ()
            spawn.with_line_callback({"xidletimer", "1000"}, {
                    stdout = function (line)
                        autolocker.ms_idle = tonumber(line)
                        autolocker:update()
                    end,
                    exit = function ()
                        print("autolocker timer exits, restarting in 5 seconds ...")
                        start_autolocker_timer()
                    end,
            })
    end)
end
                        
start_autolocker_timer()

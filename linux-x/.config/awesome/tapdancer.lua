local gtimer = require("gears.timer")
local mod = {}

function mod.create(args)
    assert(type(args.timeout) == "number")
    local ret
    ret = {
        max_ = args.max,
        timer_ = gtimer{
            timeout = args.timeout,
            autostart = false,
            call_now = false,
            single_shot = true,
            callback = function ()
                args.callback(ret.counter_)
                ret.counter_ = 0
            end
        },
        counter_ = 0,
        trigger = function ()
            ret.counter_ = ret.counter_ + 1
            if ret.counter_ >= ret.max_ then
                if ret.timer_.started then ret.timer_:stop() end
                gtimer.delayed_call(
                    function ()
                        args.callback(ret.counter_)
                        ret.counter_ = 0
                    end
                )
            else
                ret.timer_:again()
            end
        end,
        cancel = function ()
            ret.counter_ = 0
            if ret.timer_.started then ret.timer_:stop() end
        end,
        counter = function()
            return ret.counter_
        end,
    }
    return ret
end

return mod

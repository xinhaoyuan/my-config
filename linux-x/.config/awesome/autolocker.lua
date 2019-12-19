local gobject = require("gears.object")
local Locker = {}

function Locker:update()
    if self._ms_idle == nil or self._ms_before_lock == nil then
        return
    end
    if self._ms_idle < self._ms_before_lock then
        return
    end
    local now = os.time(os.date("*t"))
    if self._hold_until_timestamp ~= nil and now < self._hold_until_timestamp then
        return
    end

    self._ms_idle = nil
    self._hold_until_timestamp = nil
    self:emit_signal("trylock")
end

function Locker:set_ms_before_lock(ms_before_lock)
    self._ms_before_lock = ms_before_lock
end

function Locker:set_ms_idle(ms_idle)
    self._ms_idle = ms_idle
end

function Locker:hold(sec)
    local now = os.time(os.date("*t"))
    self._hold_until_timestamp = math.max(self._hold_until_timestamp or now, now) + sec
end

function Locker:get_hold_until_timestamp()
    if self._hold_until_timestamp == nil then
        return nil
    else
        local now = os.time(os.date("*t"))
        if self._hold_until_timestamp < now then
            return nil
        else
            return self._hold_until_timestamp
        end
    end
end

function Locker:clear_hold()
    self._hold_until_timestamp = nil
end

Locker.mt = {}
function Locker.mt.__call()
    local ret = gobject {
        class = Locker,
        enable_properties = true,
        enable_auto_signals = true,
    }
    return ret
end
setmetatable(Locker, Locker.mt)

return Locker()

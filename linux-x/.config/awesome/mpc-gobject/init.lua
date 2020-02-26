local gtimer = require("gears.timer")
local gobject = require("gears.object")
local hotpot = require("hotpot")
local mpc = require("mpc")

local klass = {}

function klass:init()
    local this = self
    self._private = {}
    self._private.mpc_conn = mpc.new(
        nil, nil, nil,
        function (err)
            this.state = { err = err }
            this:schedule_update()
        end,
        "status",
        function (_, result) this:update_status(result) end,
        "currentsong",
        function (_, result) this:update_song(result) end
    )
    self._private.ping_timer = gtimer {
        timeout = 3,
        autostart = true,
        callback = function ()
            this._private.mpc_conn:send("status", function (_, result) this:update_status(result) end)
            return true
        end
    }

end

function klass:update_status(status)
    self.status = {
        state = status.state
    }
    local time = status.time
    if time ~= nil then
        local colon_pos = time:find(":")
        if colon_pos ~= nil then
            local played = tonumber(time:sub(1, colon_pos - 1))
            local length = tonumber(time:sub(colon_pos + 1, #time))
            self.status.length = length
            self.status.played = played
            self.status.progress = played / length
        end
    end
    self:schedule_update()
end

function klass:update_song(song)
    self.song = {
        title = song.title,
        artist = song.artist,
        album = song.album,
        file = song.file,
    }
    self:schedule_update()
end

function klass:schedule_update()
    if self._private.scheduled_update then
        return
    end
    self._private.scheduled_update = true
    local this = self
    gtimer.delayed_call(function ()
            this._private.scheduled_update = false
            this:update()
    end)
end

function klass:update()
    if self.status then
        self:emit_signal("update::status", self.status)
        self.status = nil
    end

    if self.song then
        self:emit_signal("update::song", self.song)
        self.song = nil
    end
end

function klass:go_previous()
    self._private.mpc_conn:send("previous")
end

function klass:go_next()
    self._private.mpc_conn:send("next")
end

function klass:toggle_play()
    self._private.mpc_conn:toggle_play()
end

setmetatable(
    klass,
    {
        __call = function (_, args)
            args = args or {}
            local obj = gobject {
                class = klass
            }
            return obj
        end
    }
);

local mod = klass{}

hotpot.config.on_ready(
    function ()
        mod:init()
    end
)

return mod

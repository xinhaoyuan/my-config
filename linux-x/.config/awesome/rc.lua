-- awesome v4 config
-- Author: Xinhao Yuan (xinhaoyuan@gmail.com)

-- Remember certain information after client is detached but before the object is gone.
-- Need to do this before everything, so that signal handler fires before standard ones.

local capi = {
    client = client,
}

capi.client.connect_signal(
    "unmanage",
    function (c)
	c.tomb_floating = c.floating
	c.tomb_class = c.class
	c.tomb_pid = c.pid
    end
)

-- Print the error message to .xsession-errors.
awesome.connect_signal(
    "debug::error",
    function (msg)
	print(msg)
    end
)

local HOME_DIR = os.getenv("HOME")

os.execute(HOME_DIR .. "/.xdesktoprc.awesome")

local config = require("config")
require("my-autofocus")
require("orgenda").config.files = {os.getenv("HOME").."/org/TODO.org"}

require("gears.timer").delayed_call(
    function ()
	if capi.client.focus then
	    capi.client.focus:emit_signal("request::activate", "mouse.move", {raise=true})
	end
    end
                                   )

__userdata_info = {}
__userdata_info_counter = 0
setmetatable(__userdata_info, { __mode = "k" })

on_pushlightuserdata = function (userdata)
    local level = 2
    local stacktrace = {}
    while true do
        local level_info = debug.getinfo(level, "Sl")
        if not level_info then break end
        if level_info.what == "C" then
            -- Ignore C functions
        else   -- a Lua function
            if not level_info.short_src:find("/lgi/") then
                table.insert(stacktrace, { level - 1, level_info.short_src, level_info.currentline})
            end
        end
        level = level + 1
    end
    __userdata_info[userdata] = {
        id = __userdata_info_counter,
        light = true,
        timestamp = os.date("%Y%m%d%H%M%S"),
        stacktrace = stacktrace
    }
    __userdata_info_counter = __userdata_info_counter + 1
end

on_newuserdata = function (userdata)
    local level = 2
    local stacktrace = {}
    while true do
        local level_info = debug.getinfo(level, "Sl")
        if not level_info then break end
        if level_info.what == "C" then
            -- Ignore C functions
        else   -- a Lua function
            if not level_info.short_src:find("/lgi/") then
                table.insert(stacktrace, { level - 1, level_info.short_src, level_info.currentline})
            end
        end
        level = level + 1
    end
    __userdata_info[userdata] = {
        id = __userdata_info_counter,
        light = false,
        timestamp = os.date("%Y%m%d%H%M%S"),
        stacktrace = stacktrace
    }
    __userdata_info_counter = __userdata_info_counter + 1
end

require("hotpot").config.force_gc_timeout = 30

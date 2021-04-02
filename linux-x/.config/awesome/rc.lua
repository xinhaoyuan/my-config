-- awesome v4 config
-- Author: Xinhao Yuan (xinhaoyuan@gmail.com)

-- Remember certain information after client is detached but before the object is gone.
-- Need to do this before everything, so that signal handler fires before standard ones.

local capi = {
    client = client,
}

capi.client.connect_signal(
    "before::unmanage",
    function (c)
        c.tomb = true
	c.tomb_floating = c.floating
	c.tomb_class = c.class
	c.tomb_pid = c.pid
        c.tomb_geometry = c:geometry()
        c.tomb_tags = c:tags()
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

os.execute(HOME_DIR .. "/.xdesktoprc")

local config = require("config")
local beautiful = require("beautiful")
local gstring = require("gears.string")
require("my-autofocus")
local gcal_org_path = os.getenv("HOME").."/.cache/gcal.org"
os.execute("touch "..gcal_org_path)
local notix_org_path = os.getenv("HOME").."/org/notix.org"
os.execute("touch "..notix_org_path)
require("notix").config.org_file_for_pin = notix_org_path
require("orgenda").config.files = {
    {
        path = os.getenv("HOME").."/org/TODO.org",
        rank = 0,
    },
    {
        path = notix_org_path,
        rank = 1,
    },
    gcal_org_path,
                                  }
local hotpot = require("hotpot")
local fts = hotpot.focus_timestamp
local gtimer = require("gears.timer")
local awful = require("awful")

gtimer.delayed_call(
    function ()
        local clients = client.get()
        table.sort(
               clients,
               function (a, b)
                   return fts.get(a) < fts.get(b)
               end
        )
        for _, c in ipairs(clients) do
            if c:isvisible() then
                c:raise()
            end
        end
    end
)

gtimer {
    timeout = 900,
    call_now = true,
    autostart = true,
    callback = function ()
        awful.spawn({"fetch_gcal.py", "-o", gcal_org_path}, false)
    end
}

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

hotpot.config.force_gc_timeout = 30

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
require("my-menu")
require("orgenda").config.files = {os.getenv("HOME").."/org/TODO.org"}

require("gears.timer").delayed_call(
    function ()
	if capi.client.focus then
	    capi.client.focus:emit_signal("request::activate", "mouse.move", {raise=true})
	end
    end
                                   )

-- __userdata_info = {}
-- __userdata_info_counter = 0
-- setmetatable(__userdata_info, { __mode = "k" })

-- on_pushlightuserdata = function (userdata)
--     local level = 2
--     local info = {}
--     while true do
--         local level_info = debug.getinfo(level, "Sl")
--         if not level_info then break end
--         if level_info.what == "C" then
--             -- Ignore C functions
--         else   -- a Lua function
--             if not level_info.short_src:find("/lgi/") then
--                 table.insert(info, {level - 1, level_info.short_src, level_info.currentline})
--             end
--         end
--         level = level + 1
--     end
--     __userdata_info[userdata] = {__userdata_info_counter, true, os.date("%Y%m%d%H%M%S"), info}
--     __userdata_info_counter = __userdata_info_counter + 1
-- end

-- on_newuserdata = function (userdata)
--     local level = 2
--     local info = {}
--     while true do
--         local level_info = debug.getinfo(level, "Sl")
--         if not level_info then break end
--         if level_info.what == "C" then
--             -- Ignore C functions
--         else   -- a Lua function
--             if not level_info.short_src:find("/lgi/") then
--                 table.insert(info, {level - 1, level_info.short_src, level_info.currentline})
--             end
--         end
--         level = level + 1
--     end
--     __userdata_info[userdata] = {__userdata_info_counter, false, os.date("%Y%m%d%H%M%S"), info}
--     __userdata_info_counter = __userdata_info_counter + 1
-- end

-- dump_userdata_info = function ()
--     collectgarbage("collect")
--     collectgarbage("collect")
--     collectgarbage("collect")
--     output = io.open(os.getenv("HOME").."/awesome_userdata_info", "w")
--     local count = 0
--     local ud_list = {}
--     for ud, ts_and_info in pairs(__userdata_info) do
--         table.insert(ud_list, {ud, ts_and_info[1], ts_and_info[2], ts_and_info[3], ts_and_info[4]})
--     end

--     table.sort(ud_list, function (a, b) return a[4] < b[4] end)
--     for _, tp in ipairs(ud_list) do
--         local ud, id, is_light, ts, info = table.unpack(tp)
--         output:write("#"..tostring(id)..(is_light and " l " or " h ")..tostring(ts).." "..tostring(ud).."\n")
--         for i, l in ipairs(info) do
--             output:write("--> "..tostring(l[1]).." "..tostring(l[2]).." "..tostring(l[3]).."\n")
--         end
--     end
--     output:write(tostring(#ud_list).." userdata dumped")
--     output:close()
-- end

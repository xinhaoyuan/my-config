-- modified from lib/awful/autofocus.lua, version git

---------------------------------------------------------------------------
--- Autofocus functions.
--
-- When loaded, this module makes sure that there's always a client that will
-- have focus on events such as tag switching, client unmanaging, etc.
--
-- @author Julien Danjou &lt;julien@danjou.info&gt;
-- @copyright 2009 Julien Danjou
-- @module awful.autofocus
---------------------------------------------------------------------------

local capi = {
    mouse = mouse,
    client = client,
    screen = screen,
    tag = tag,
}

local awful = require("awful")
local awful_client = require("awful.client")
local gtimer = require("gears.timer")
local fts = require("hotpot").focus_timestamp

local module = {
    debug = false,
    enabled = true,
}

function module.find_alternative_focus(prev, s)
    if module.debug then
        print("== DEBUG == called find_alternative_focus", prev and prev.valid and tostring(prev), s)
    end
    local clients = {}
    for c in awful_client.iterate(
        function (c)
            return c:isvisible() and (c.type == "desktop" or awful_client.focus.filter(c))
        end
    ) do
        clients[#clients + 1] = c
    end
    table.sort(
        clients,
        function (a, b)
            if a.type == "desktop" then
                return false
            elseif b.type == "desktop" then
                return true
            else
                return fts.get(a) > fts.get(b)
            end
        end
    )
    if module.debug then
        print("== DEBUG == alternative focus candidate")
        for i, c in ipairs(clients) do
            print("== DEBUG ==", i, "fts", fts.get(c), "type", c.type, "screen", c.screen, "client", c)
        end
    end
    if #clients == 0 then
        return nil
    end
    local ret_index = 1
    for i, c in ipairs(clients) do
        if c.screen == s then
            ret_index = i
            break
        end
    end
    if module.debug then
        print("== DEBUG == alternative focus index ", ret_index)
    end
    return clients[ret_index]
end

local managed_counter = 0

--- Give focus when clients appear/disappear.
--
-- @param prev the previous focus client, may not be valid now
-- @param s the screen of prev, in case prev.screen is not accessible now
local function check_focus(prev, s)
    if module.debug then
        print("== DEBUG == check focus on counter", managed_counter, prev, s, s and s.valid, capi.client.focus)
    end
    if managed_counter > 0 or not module.enabled then return end
    if not s or not s.valid then return end
    -- When no visible client has the focus, or the screen is not where the mouse is and there is no previous focus.
    if not capi.client.focus or not capi.client.focus:isvisible() or not awful_client.focus.filter(capi.client.focus)
        or (s == capi.mouse.screen and capi.client.focus.screen ~= s and (prev == nil or not prev.valid))
    then
        local c = module.find_alternative_focus(prev, s)
        if c then
            awful_client.focus.history.disable_tracking()
            c:emit_signal("request::activate", "autofocus.check_focus",
                          {raise=false})
            c:raise()
            awful_client.focus.history.enable_tracking()
        end
    end
end

--- Check client focus (delayed).
-- @param obj An object that should have a .screen property.
local function check_focus_delayed(obj)
    gtimer.delayed_call(
        function ()
            if not obj.valid then return end
            check_focus(obj, obj.screen)
        end)
end

--- Give focus on tag selection change.
--
-- @param tag A tag object
local function check_focus_tag(t)
    if managed_counter > 0 then return end
    check_focus(nil, t.screen)
end

function module.manage_focus(s)
    if module.debug then
        print("== DEBUG == manage focus on counter", managed_counter, "screen", s)
    end
    managed_counter = managed_counter + 1
end

function module.unmanage_focus(s)
    if module.debug then
        print("== DEBUG == unmanage focus on counter", managed_counter, "screen", s)
    end
    if managed_counter > 0 then
        managed_counter = managed_counter - 1
        if managed_counter == 0 then
            gtimer.delayed_call(function () check_focus(nil, s) end)
        end
    else
        print("!!! unpared unmanage_focus !!!", debug.traceback())
    end
end

capi.tag.connect_signal(
    "property::selected", function (t)
        gtimer.delayed_call(function () check_focus_tag(t) end)
    end)

capi.client.connect_signal("unfocus",             check_focus_delayed)
capi.client.connect_signal("unmanage",            function (c) check_focus(c, c.screen) end)
capi.client.connect_signal("tagged",              check_focus_delayed)
capi.client.connect_signal("untagged",            check_focus_delayed)
capi.client.connect_signal("property::hidden",    check_focus_delayed)
capi.client.connect_signal("property::minimized", check_focus_delayed)
capi.client.connect_signal("property::sticky",    check_focus_delayed)

return module

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

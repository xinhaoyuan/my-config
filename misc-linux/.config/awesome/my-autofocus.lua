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

local client = client
local aclient = require("awful.client")
local timer = require("gears.timer")
local cf = require("cyclefocus")

local find_alternative_focus = function (prev, s)
   return cf.find_history(
      0, {
         function (c)
            return c.screen == s and aclient.focus.filter(c)
         end
   })
end

local autofocus = {
   find_alternative_focus = find_alternative_focus
}

--- Give focus when clients appear/disappear.
--
-- @param prev the previous focus client, may not be valid now
-- @param s the screen of prev, in case prev.screen is not accessible now
local function check_focus(prev, s)
    if not s or not s.valid then return end
    -- When no visible client has the focus...
    if not client.focus or not client.focus:isvisible() or not aclient.focus.filter(client.focus) then
        local c = autofocus.find_alternative_focus(prev, s)
        if c then
            -- raise the client in "request::activate" will set the urgent flag when switching tag
            c:emit_signal("request::activate", "autofocus.check_focus",
                          {raise=false})
            c:raise()
        end
    end
end

--- Check client focus (delayed).
-- @param obj An object that should have a .screen property.
local function check_focus_delayed(obj)
    timer.delayed_call(check_focus, obj, obj.screen)
end

--- Give focus on tag selection change.
--
-- @param tag A tag object
local function check_focus_tag(t)
    local s = t.screen
    if (not s) or (not s.valid) then return end
    s = screen[s]
    check_focus(nil, s)
    if not client.focus or not aclient.focus.filter(client.focus) or screen[client.focus.screen] ~= s then
        local c = aclient.focus.history.get(s, 0, aclient.focus.filter)
        if c then
            -- raise the client in "request::activate" will set the urgent flag when switching tag
            c:emit_signal("request::activate", "autofocus.check_focus_tag",
                          {raise=false})
            c:raise()
        end
    end
end

tag.connect_signal("property::selected", function (t)
    timer.delayed_call(check_focus_tag, t)
end)

client.connect_signal("unfocus",             check_focus_delayed)
client.connect_signal("unmanage",            function (c) check_focus(c, c.screen) end)
client.connect_signal("tagged",              check_focus_delayed)
client.connect_signal("untagged",            check_focus_delayed)
client.connect_signal("property::hidden",    check_focus_delayed)
client.connect_signal("property::minimized", check_focus_delayed)
client.connect_signal("property::sticky",    check_focus_delayed)

return autofocus

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

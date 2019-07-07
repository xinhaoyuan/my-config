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

local api = {
   client = client,
   awful_client = require("awful.client"),
   timer = require("gears.timer"),
   focus = require("my-focus"),
}

local find_alternative_focus = function (prev, s)
   return api.focus.match_in_history(
      {
         function (c)
            return c.screen == s and api.awful_client.focus.filter(c)
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
    if not api.client.focus or not api.client.focus:isvisible() or not api.awful_client.focus.filter(api.client.focus) then
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
    api.timer.delayed_call(check_focus, obj, obj.screen)
end

--- Give focus on tag selection change.
--
-- @param tag A tag object
local function check_focus_tag(t)
    local s = t.screen
    if (not s) or (not s.valid) then return end
    s = screen[s]
    check_focus(nil, s)
    if not api.client.focus or not api.awful_client.focus.filter(api.client.focus) or screen[api.client.focus.screen] ~= s then
        local c = api.awful_client.focus.history.get(s, 0, api.awful_client.focus.filter)
        if c then
            -- raise the client in "request::activate" will set the urgent flag when switching tag
            c:emit_signal("request::activate", "autofocus.check_focus_tag",
                          {raise=false})
            c:raise()
        end
    end
end

tag.connect_signal("property::selected", function (t)
    api.timer.delayed_call(check_focus_tag, t)
end)

api.client.connect_signal("unfocus",             check_focus_delayed)
api.client.connect_signal("unmanage",            function (c) check_focus(c, c.screen) end)
api.client.connect_signal("tagged",              check_focus_delayed)
api.client.connect_signal("untagged",            check_focus_delayed)
api.client.connect_signal("property::hidden",    check_focus_delayed)
api.client.connect_signal("property::minimized", check_focus_delayed)
api.client.connect_signal("property::sticky",    check_focus_delayed)

return autofocus

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

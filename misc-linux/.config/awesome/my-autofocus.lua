-- modified from lib/awful/autofocus.lua, version 3.5.9

local client = client
local screen = screen
local aclient = require("awful.client")
local atag = require("awful.tag")
local cf = require("cyclefocus")

local find_alternative_focus = function (src_c)
   return cf.find_history(
      0, {
         function (c)
            return c.screen == src_c.screen and aclient.focus.filter(c)
         end
   })
end

local autofocus = {
   find_alternative_focus = find_alternative_focus
}

--- When loaded, this module makes sure that there's always a client that will have focus
-- on events such as tag switching, client unmanaging, etc.
-- awful.autofocus

-- Give focus when clients appear/disappear.
-- @param obj An object that should have a .screen property.
local function check_focus(obj)
   -- When no visible client has the focus...
   if not client.focus or not client.focus:isvisible() then
      local c = autofocus.find_alternative_focus(obj)
      if c then client.focus = c end
   end
end

-- Give focus on tag selection change.
-- @param tag A tag object
local function check_focus_tag(t)
   local s = atag.getscreen(t)
   if not s then return end
   check_focus({ screen = s })
   if client.focus and client.focus.screen ~= s then
      local c = aclient.focus.history.get(s, 0)
      if c then client.focus = c end
   end
end

atag.attached_connect_signal(nil, "property::selected", check_focus_tag)
client.connect_signal("unmanage", check_focus)
client.connect_signal("tagged", check_focus)
client.connect_signal("untagged", check_focus)
client.connect_signal("property::hidden", check_focus)
client.connect_signal("property::minimized", check_focus)

return autofocus


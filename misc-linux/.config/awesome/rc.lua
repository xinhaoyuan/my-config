require("my-utils")
local aw = require("awful")
local ar = require("awful.rules")
local na = require("naughty")
local be = require("beautiful")
local wi = require("wibox")
-- 3rd party libs
local af = require("my-autofocus")
local sc = require("my-ui-scale")
local cf = require("cyclefocus")

na.config.defaults.font = "Sans " .. (10 * sc.factor)

local debug = function (msg)
   na.notify({
         text = tostring(msg),
         timeout = 10
   })
end

awesome.connect_signal(
   "debug::error",
   function (msg)
      debug(msg)
   end
)

aw.util.spawn_with_shell("$HOME/.xdesktoprc.awesome")
be.init("/usr/share/awesome/themes/default/theme.lua")

-- layouts
local layouts = {
   aw.layout.suit.tile,
   aw.layout.suit.fair,
   aw.layout.suit.max
}

tags = {}
for s = 1, screen.count() do
   tags[s] = aw.tag({ 1, 2 ,3, 4 }, s, layouts[1])
end

-- reserve room for conky

dummy_status_bar = aw.wibox({ position = "top", screen = 1, ontop = false, width = 1, height = 20 * sc.factor })

-- and bottom wibox for task list and tray

local my_wibox = {}
local my_tag_list = {}
local my_task_list = {}
local my_tray = wi.widget.systray()

my_tray:set_base_size(20 * sc.factor)

my_tag_list.buttons = aw.util.table.join(
   aw.button({ }, 1, aw.tag.viewonly),
   aw.button({ "Mod4" }, 1, aw.client.movetotag),
   aw.button({ }, 3, aw.tag.viewtoggle),
   aw.button({ "Mod4" }, 3, aw.client.toggletag),
   aw.button({ }, 4, function(t) aw.tag.viewnext(aw.tag.getscreen(t)) end),
   aw.button({ }, 5, function(t) aw.tag.viewprev(aw.tag.getscreen(t)) end)
)

my_task_list.buttons = aw.util.table.join(
   aw.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() then
               aw.tag.viewonly(c:tags()[1])
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   aw.button({ }, 3, function ()
         if instance then
            instance:hide()
            instance = nil
         else
            instance = aw.menu.clients({ width=500 })
         end
   end),
   aw.button({ }, 4, function ()
         aw.client.focus.byidx(1)
         if client.focus then client.focus:raise() end
   end),
   aw.button({ }, 5, function ()
         aw.client.focus.byidx(-1)
         if client.focus then client.focus:raise() end
end))

for s = 1, screen.count() do
   my_task_list[s] = aw.widget.tasklist.new(
      s,
      aw.widget.tasklist.filter.currenttags,
      my_task_list.buttons,
      {
         font = "Sans " .. (10 * sc.factor)
      }
   )

   my_tag_list[s] = aw.widget.taglist(
      s, aw.widget.taglist.filter.all, my_tag_list.buttons,
      {
         font = "Sans " .. (10 * sc.factor)
      }
   )
   
   my_wibox[s] = aw.wibox({
         screen = s,
         fg = be.fg_normal,
         bg = be.bg_normal,
         height = 20 * sc.factor,
         position = "bottom",
         border_width = 0,
   })

   local left_layout = wi.layout.fixed.horizontal()
   left_layout:add(my_tag_list[s])

   local right_layout = wi.layout.fixed.horizontal()
   right_layout:add(my_tray)

   local layout = wi.layout.align.horizontal()
   layout:set_left(left_layout)
   layout:set_middle(my_task_list[s])
   layout:set_right(right_layout)
   
   my_wibox[s]:set_widget(layout)
end

-- helper functions

local is_floating = function (c)
   return aw.client.floating.get(c) or c.maximized_vertical or c.maximized_horizontal or c.type == "dialog"
end

af.find_alternative_focus = function(c)   
   local f = is_floating(c)
   local new_focus = cf.find_history(
      0, {
         function (nc)
            return
               nc ~= c
               and cf.filters.same_screen(nc, c)
               and is_floating(nc) == f
         end
   })
   return new_focus
end

-- keys and buttons
local global_keys = aw.util.table.join(
   aw.key({ "Mod4", "Control" }, "Left", aw.tag.viewprev),
   aw.key({ "Mod4", "Control" }, "Right", aw.tag.viewnext),
      
   aw.key({ "Mod4" }, "[", function () aw.layout.inc(layouts, -1) end),
   aw.key({ "Mod4" }, "]", function () aw.layout.inc(layouts, 1) end),

   aw.key({ "Mod4" }, "w", function () aw.client.focus.bydirection("up"); client.focus:raise() end),
   aw.key({ "Mod4" }, "a", function () aw.client.focus.bydirection("left"); client.focus:raise() end),
   aw.key({ "Mod4" }, "s", function () aw.client.focus.bydirection("down"); client.focus:raise() end),
   aw.key({ "Mod4" }, "d", function () aw.client.focus.bydirection("right"); client.focus:raise() end),

   aw.key({ "Mod4" }, "j", function () aw.tag.incmwfact(-0.05) end),
   aw.key({ "Mod4" }, "l", function () aw.tag.incmwfact( 0.05) end),
   aw.key({ "Mod4" }, "i", function () aw.client.incwfact(-0.1) end),
   aw.key({ "Mod4" }, "k", function () aw.client.incwfact( 0.1) end),

   aw.key({ }, "XF86AudioLowerVolume", function() aw.util.spawn("amixer sset Master,0 2%-") end),
   aw.key({ }, "XF86AudioRaiseVolume", function() aw.util.spawn("amixer sset Master,0 2%+") end),
   aw.key({ }, "XF86AudioMute", function() aw.util.spawn("amixer sset Master,0 toggle") end),
   aw.key({ }, "XF86MonBrightnessUp", function () aw.util.spawn("xbacklight -inc 5") end),
   aw.key({ }, "XF86MonBrightnessDown", function () aw.util.spawn("xbacklight -dec 5") end),   

   aw.key({ "Mod4", "Control" }, "m", function () for _, c in pairs(client.get()) do c.minimized = false end end),

   aw.key({ "Mod4" }, "r", function () aw.util.spawn_with_shell("dlauncher open") end),
   aw.key({ "Mod4" }, "Return", function () aw.util.spawn("open-terminal-emulator") end),
   aw.key({ "Mod4" }, "t", function () aw.util.spawn("urxvt -name root-terminal") end),
   
   aw.key({ "Mod4", "Control" }, "Escape", awesome.quit)
)

local client_keys = aw.util.table.join(
   cf.key({ "Mod1" }, "Tab", 1,
      {
         modifier = "Alt_L",
         cycle_filters = {
            cf.filters.same_screen, cf.filters.common_tag
            ,
            function (c, src_c)
               if c.pid == src_c.pid then return true
               else
                  return is_floating(c) == is_floating(src_c)
               end
            end
         }
   }),
   aw.key({ "Mod4" }, "Tab", function(src_c)
         local f = is_floating(src_c)
         local new_focus = nil
         
         for _, c in ipairs(client.get(src_c.screen)) do
            if c:isvisible() and (not aw.client.focus.filter or aw.client.focus.filter(c)) then
               if not is_floating(c) then
                  if f then
                     c:raise()
                  else
                     c:lower()
                  end
               else
               end
            end            
         end

         new_focus = cf.find_history(
            0, {
               function (c)
                  return cf.filters.same_screen(c, src_c)
                     and cf.filters.common_tag(c, src_c)
                     and is_floating(c) ~= f
               end
         })
         
         if new_focus then
            client.focus = new_focus
         end
   end),

   aw.key({ "Mod4" }, "Up", function (c)
         if c.minimized then
            c.minimized = false
         else
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical = not c.maximized_vertical
         end
   end),
   aw.key({ "Mod4" }, "Down", function (c)
         c.minimized = true
   end),
      
   aw.key({ "Mod4" }, "Left", function (c)
         if not is_floating(c) then
            c:swap(aw.client.getmaster())
         end
   end),

   aw.key({ "Mod4" }, "Right", function (c) aw.client.floating.toggle(c) end),

   aw.key({ "Mod4" }, "c", function (c) c:kill() end)
)

local client_buttons = aw.util.table.join(
   aw.button({ }, 1, function (c) client.focus = c; c:raise() end),
   aw.button({ "Mod4" }, 1, aw.mouse.client.move),
   aw.button({ "Mod4" }, 3, aw.mouse.client.resize)
)

root.keys(global_keys)

-- rules

client.connect_signal(
   "focus",
   function (c)
      c.border_color = be.border_focus
   end
)
client.connect_signal(
   "unfocus",
   function (c)
      c.border_color = be.border_normal      
   end
)

ar.rules = {
   {
      rule = { },
      properties = {
         focus = true,
         size_hints_honor = false,
         keys = client_keys,
         buttons = client_buttons,
         border_width = 4,
         border_color = be.border_normal
      }
   },
   {
      rule = { class = "Conky" },
      properties = {
         floating = true,
         sticky = true,
         ontop = false,
         below = true,
         focus = false,
         border_width = 0,
         focusable = false
      }
   },
}

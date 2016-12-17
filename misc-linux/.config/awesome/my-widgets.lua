local aw = require("awful")
local be = require("beautiful")
local wi = require("wibox")
local sc = require("my-ui-scale")

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
         font = "Sans " .. (10 * sc.font_factor)
      }
   )

   my_tag_list[s] = aw.widget.taglist(
      s, aw.widget.taglist.filter.all, my_tag_list.buttons,
      {
         font = "Sans " .. (10 * sc.font_factor)
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

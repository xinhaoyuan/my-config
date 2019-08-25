local capi = {
   screen = screen,
   client = client,
}
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local lgi = require("lgi")
local dpi = require("beautiful.xresources").apply_dpi

-- A waffle view is a table with the following elements
--   .widget -- the entry widget
--   .key_handler (optional) -- the key handling function. It returns a boolean if the key event is captured.

local function create_view(args)
   local view = {}
   view.keys = {}

   local rows = wibox.widget {
      spacing = dpi(10),
      layout = wibox.layout.fixed.vertical,
   }
   for i, r in ipairs(args.rows or {}) do
      local row_layout = wibox.widget {
         spacing = dpi(10),
         layout = wibox.layout.fixed.horizontal,
      }
      for j, cell in ipairs(r) do
         if cell.key_handler then
            for _, k in ipairs(cell.keys or {}) do
               view.keys[k] = cell.key_handler 
            end
         end
         row_layout:add(cell.widget)
      end
      rows:add(row_layout)
   end

   view.widget = rows

   view.key_handler = function (mod, key, event)
      if event == "press" and view.keys[key] then
         view.keys[key](mod, key, event)
         return true
      else
         return false
      end
   end

   return view
end

local waffle = {
   create_view = create_view,
   gravity_ = "southwest",
   widget_container = wibox.widget {
      bg = beautiful.bg_normal,
      widget = wibox.container.background,
   },
}

function waffle:update_layout()   
   if self.gravity_ == "center" then
      self.wibox_.widget = wibox.widget {
         nil,
         {
            nil, self.widget_container, nil,
            expand = "outside",
            layout = wibox.layout.align.horizontal,
         }, nil,
         expand = "outside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "north" then
      self.wibox_.widget = wibox.widget {
         {
            nil, self.widget_container, nil,
            expand = "outside",
            layout = wibox.layout.align.horizontal,
         }, nil, nil,
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "south" then
      self.wibox_.widget = wibox.widget {
         nil, nil,
         {
            nil, self.widget_container, nil,
            expand = "outside",
            layout = wibox.layout.align.horizontal,
         },
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "west" then
      self.wibox_.widget = wibox.widget {
         nil,
         {
            self.widget_container, nil, nil,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         }, nil,
         expand = "outside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "east" then
      self.wibox_.widget = wibox.widget {
         nil,
         {
            nil, nil, self.widget_container,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         }, nil,
         expand = "outside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "northeast" then
      self.wibox_.widget = wibox.widget {
         {
            nil, nil, self.widget_container,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         }, nil, nil,
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "northwest" then
      self.wibox_.widget = wibox.widget {
         {
            self.widget_container, nil, nil,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         }, nil, nil,
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "southeast" then
      self.wibox_.widget = wibox.widget {
         nil, nil,
         {
            nil, nil, self.widget_container,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         },
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   elseif self.gravity_ == "southwest" then
      self.wibox_.widget = wibox.widget {
         nil, nil,
         {
            self.widget_container, nil, nil,
            expand = "inside",
            layout = wibox.layout.align.horizontal,
         },
         expand = "inside",
         layout = wibox.layout.align.vertical,
      }
   end
end

function waffle:set_gravity(gravity)
   if self.gravity_ ~= gravity then
      self.gravity_ = gravity
      self:update_layout()
   end
end

function waffle:show(view, push, screen)
   screen = screen or awful.screen.focused()
   if self.wibox_ == nil then
      self.wibox_ = wibox({
            screen = screen,
            x = screen.workarea.x,
            y = screen.workarea.y,
            width = screen.workarea.width,
            height = screen.workarea.height,
            bg = "#00000000",
            opacity = 1,
            ontop = true,
            type = "dock",
      })

      self:update_layout()
   else
      self.wibox_:geometry({
            x = screen.workarea.x,
            y = screen.workarea.y,
            width = screen.workarea.width,
            height = screen.workarea.height,
      })
   end

   if push then
      self.stack_ = self.stack_ or {}
      table.insert(self.stack_, self.view_)
   end
   self.view_ = view
   self.widget_container.widget = view.widget 

   if not self.wibox_.visible then
      self.keygrabber_ = awful.keygrabber.run(
         function (mod, key, event)
            if self.view_.key_handler and self.view_.key_handler(mod, key, event) then
               -- pass
            elseif event == "press" then
               if key == "Escape" or key == "F12" then
                  self:hide()
               elseif key == "BackSpace" then
                  self:go_back()
               end
            end
         end
      )
      self.wibox_.visible = true
   end
end

function waffle:go_back()
   local headpos = self.stack_ and #self.stack_ or 0
   if headpos >= 1 then
      local last = self.stack_[headpos]
      table.remove(self.stack_, headpos)
      self:show(last, nil, false)
   else
      self:hide()
   end
end

function waffle:hide()
   if self.keygrabber_ ~= nil then
      awful.keygrabber.stop(self.keygrabber_)
      self.keygrabber_ = nil
   end
   if self.wibox_ ~= nil then
      self.wibox_.visible = false
      self.wibox_ = nil
   end
   self.view_ = nil
   self.stack_ = nil
end

return waffle

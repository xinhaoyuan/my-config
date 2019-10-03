local base = require("wibox.widget.base")
local gtable = require("gears.table")

local fallback = { mt = {} }

function fallback:layout(context, width, height)
   local w = 0
   local h = 0

   local ret = {}
   for i, v in ipairs(self._private.children) do
      w, h = base.fit_widget(self, context, v, width, height)
      if self._private.debug_prefix then
         print(self._private.debug_prefix .. "#" .. tostring(i) .. " " .. tostring(w) .. " " .. tostring(h))
      end
      if (w > 0 and h > 0) or (i == #self._private.children and self._private.draw_last) then
         table.insert(ret, base.place_widget_at(v, 0, 0, width, height))
         break
      else
         table.insert(ret, base.place_widget_at(v, 0, 0, 0, 0))
      end
   end
   return ret
end

function fallback:fit(context, width, height)
   local w = 0
   local h = 0
   for _, v in ipairs(self._private.children) do
      w, h = base.fit_widget(self, context, v, width, height)
      if w > 0 and h > 0 then
         return w, h
      end
   end
   return 0, 0
end

function fallback:get_children()
   return self._private.children
end

function fallback:set_children(children)
   self._private.children = children
   self:emit_signal("widget::layout_changed")
end

function fallback:set_draw_last(dl)
   self._private.draw_last = dl
   self:emit_signal("widget::layout_changed")
end

function fallback:set_debug_prefix(prefix)
   self._private.debug_prefix = prefix
end

function fallback:new(children, draw_last)
   local ret = base.make_widget(nil, nil, {enable_properties = true})
   gtable.crush(ret, self, true)

   ret._private.children = children or {}
   if draw_last ~= nil then
      ret._private.draw_last = draw_last
   end
   return ret
end

function fallback.mt:__call(...)
   return self:new(...)
end

setmetatable(fallback, fallback.mt)

return fallback

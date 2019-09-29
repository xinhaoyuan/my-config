local base = require("wibox.widget.base")
local gtable = require("gears.table")

local placeholder = { mt = {} }

function placeholder:layout(context, width, height)
   local w = 0
   local h = 0

   local ret = {}
   if self._private.main then
      w, h = base.fit_widget(self, context, self._private.main, width, height)
   end
   if w > 0 and h > 0 then
      table.insert(ret, base.place_widget_at(self._private.main, 0, 0, width, height))
   else
      if self._private.main then
         table.insert(ret, base.place_widget_at(self._private.main, 0, 0, 0, 0))
      end
      if self._private.alt then
         table.insert(ret, base.place_widget_at(self._private.alt, 0, 0, width, height))
      end
   end

   return ret
end

function placeholder:fit(context, width, height)
   local w = 0
   local h = 0
   if self._private.main then
      w, h = base.fit_widget(self, context, self._private.main, width, height)
   end
   if self._private.alt and (w == 0 or h == 0) then
      w, h = base.fit_widget(self, context, self._private.alt, width, height)
   end
   return w, h
end

function placeholder:get_alt()
   return self._private.alt
end

function placeholder:set_alt(alt)
   self._private.alt = base.make_widget_from_value(alt)
   self:emit_signal("widget::layout_changed")
end

function placeholder:get_main()
   return self._private.main
end

function placeholder:set_main(main)
   self._private.main = base.make_widget_from_value(main)
   self:emit_signal("widget::layout_changed")
end

function placeholder:new(main, alt)
   local ret = base.make_widget(nil, nil, {enable_properties = true})
   gtable.crush(ret, self, true)

   if main ~= nil then self:set_main(main) end
   if alt ~= nil then self:set_alt(alt) end

   return ret
end

function placeholder.mt:__call(...)
   return self:new(...)
end

setmetatable(placeholder, placeholder.mt)

return placeholder

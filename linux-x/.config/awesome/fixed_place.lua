-- Fix a flaw in place

local base = require("wibox.widget.base")

function fixed_place(place)
   function place:fit(context, width, height)
      if not self._private.widget then
         return 0, 0
      end
      
      local w, h = base.fit_widget(self, context, self._private.widget, width, height)
      
      return (self._private.fill_horizontal and width or w), (self._private.fill_vertical and height or h)
   end
   return place
end

return fixed_place

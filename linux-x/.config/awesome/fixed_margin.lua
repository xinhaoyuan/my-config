-- Fix a flaw in margin which make child updates stop propagating.

local base = require("wibox.widget.base")

function fixed_margin(margin)
   function margin:layout(_, width, height)
      if self._private.widget then
         local x = self._private.left
         local y = self._private.top
         local w = self._private.right
         local h = self._private.bottom
         
         local resulting_width = width - x - w
         local resulting_height = height - y - h
         
         return { base.place_widget_at(self._private.widget, x, y, math.max(0, resulting_width), math.max(0, resulting_height)) }
      end
   end
   return margin
end

return fixed_margin

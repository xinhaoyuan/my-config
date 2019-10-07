-- A textbox Hack to draw a outline.

local gcolor = require("gears.color")
local lgi = require("lgi")

return function(textbox, highlight_color, highlight_width)
   highlight_color = gcolor(highlight_color)
   highlight_width = highlight_width or 1
   textbox.orig_draw = textbox.draw
   function textbox:draw(context, cr, width, height)
      ncr = {}
      ncr.mt = {}
      ncr.mt.__index = function(_, key)
         if key == "show_layout" then
            ncr[key] = function (self, layout)
               local p = cr:copy_path()
               cr:save()
               cr:layout_path(layout)
               cr:set_source(highlight_color)
               cr:set_line_width(highlight_width)
               cr:set_line_join("ROUND")
               cr:stroke()
               cr:restore()
               cr:new_path()
               cr:append_path(p)
               cr:show_layout(layout)
            end
         else
            ncr[key] = function (self, ...)
               cr[key](cr, ...)
            end
         end

         return ncr[key]
      end
      setmetatable(ncr, ncr.mt)
      textbox:orig_draw(context, ncr, width, height)
   end
   return textbox
end

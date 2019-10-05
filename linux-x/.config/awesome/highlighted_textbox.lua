local gcolor = require("gears.color")

return function(textbox, highlight_color, highlight_width)
   highlight_color = gcolor(highlight_color)
   highlight_width = highlight_width or 1
   textbox.orig_draw = textbox.draw
   function textbox:draw(context, cr, width, height)
      ncr = {}
      ncr.mt = {}
      ncr.mt.__index = function(_, key)
         if key == "show_layout" then
            return function (self, layout)
               cr:save()
               cr:layout_path(layout)
               local p = cr:copy_path()
               cr:set_source(highlight_color)
               cr:set_line_width(highlight_width)
               cr:stroke()
               cr:restore()
               cr:new_path()
               cr:append_path(p)
               cr:fill()
            end
         else
            return function (self, ...)
               cr[key](cr, ...)
            end
         end
      end
      setmetatable(ncr, ncr.mt)
      textbox:orig_draw(context, ncr, width, height)
   end
   return textbox
end

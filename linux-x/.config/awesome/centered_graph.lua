local gtable = require("gears.table")
local gcolor = require("gears.color")
local graph = require("wibox.widget.graph")

local centered_graph = {mt={}}

function centered_graph:new(...)
   local ret = graph(...) 
   gtable.crush(ret, self, true)
   return ret
end

-- copied from wibox.widget.graph
function centered_graph.draw(self, _, cr, width, height)
    local max_value = self._private.max_value
    local min_value = self._private.min_value or (
        self._private.scale and math.huge or 0)
    local values = self._private.values

    local step_shape = self._private.step_shape
    local step_spacing = self._private.step_spacing or 0
    local step_width = self._private.step_width or 1

    cr:set_line_width(1)

    -- Draw the background first
    cr:set_source(gcolor(self._private.background_color or beautiful.graph_bg or "#000000aa"))
    cr:paint()

    -- Account for the border width
    cr:save()
    if self._private.border_color then
        cr:translate(1, 1)
        width, height = width - 2, height - 2
    end

    if self._private.scale then
       for _, v in ipairs(values) do
          if v > max_value then
             max_value = v
          end
          if min_value > v then
             min_value = v
          end
       end
    end

    -- Draw the background on no value
    if #values ~= 0 then
       -- Draw reverse
       for i = 0, #values - 1 do
          local value = values[#values - i]
          if value >= 0 then
             local x = i*step_width + ((i-1)*step_spacing) + 0.5
             value = (value - min_value) / max_value
             local start = height * (1 - value) / 2
             local length = height * value
             cr:move_to(x, start)

             if step_shape then
                cr:translate(step_width + (i>1 and step_spacing or 0), start)
                step_shape(cr, step_width, length)
                cr:translate(0, -(start))
             elseif step_width > 1 then
                cr:rectangle(x, start, step_width, length)
             else
                cr:line_to(x, start + length)
             end
          end
       end
       cr:set_source(gcolor(self._private.color or beautiful.graph_fg or "#ff0000"))

       if step_shape or step_width > 1 then
          cr:fill()
       else
          cr:stroke()
       end
    end

    -- Undo the cr:translate() for the border and step shapes
    cr:restore()

    -- Draw the border last so that it overlaps already drawn values
    if self._private.border_color then
        -- We decremented these by two above
        width, height = width + 2, height + 2

        -- Draw the border
        cr:rectangle(0.5, 0.5, width - 1, height - 1)
        cr:set_source(gcolor(self._private.border_color or beautiful.graph_border_color or "#ffffff"))
        cr:stroke()
    end
end


function centered_graph.mt:__call(...)
   return self:new(...)
end

setmetatable(centered_graph, centered_graph.mt)

return centered_graph

local base = require("wibox.widget.base")
local gtable = require("gears.table")

local compactor = {}

function compactor:layout(context, width, height)
    local child = self._private.children[1]
    if child == nil then return {} end
    return {base.place_widget_at(child, 0, 0, width, height)}
end

local SIZE_MAX = 100000000
function compactor:fit(context, width, height)
    local child = self._private.children[1]
    if child == nil or width == 0 or height == 0 then return 0, 0 end
    local w, h
    repeat
        if self._private.horizontal then
            w, h = base.fit_widget(self, context, child, width, SIZE_MAX)
            if h > height then break end
            if self._private.stretch_first then height = h end
            local width_max = width
            local width_min = 1
            while width_max - width_min > 0 do
                local twidth = math.floor((width_max + width_min) / 2)
                local tw, th = base.fit_widget(self, context, child, twidth, SIZE_MAX)
                if tw <= twidth and th <= height then
                    width_max = tw
                    w = tw
                    h = th
                else
                    width_min = twidth + 1
                end
            end
        else
            w, h = base.fit_widget(self, context, child, SIZE_MAX, height)
            if w > width then break end
            if self._private.stretch_first then width = w end
            local height_max = height
            local height_min = 1
            while height_max - height_min > 0 do
                local theight = math.floor((height_max + height_min) / 2)
                local tw, th = base.fit_widget(self, context, child, SIZE_MAX, theight)
                if tw <= width and th <= theight then
                    height_max = th
                    w = tw
                    h = th
                else
                    height_min = theight + 1
                end
            end
        end
    until true
    if w == 0 or h == 0 then return 0, 0 end
    return math.min(w, width), math.min(h, height)
end

function compactor:get_children()
    return self._private.children
end

function compactor:set_children(children)
    self._private.children = children
    self:emit_signal("widget::layout_changed")
end

function compactor:set_horizontal(h)
    if self._private.horizontal ~= h then
        self._private.horizontal = h
        self:emit_signal("widget::layout_changed")
    end
end

function compactor.new(...)
    local ret = base.make_widget(nil, nil, {enable_properties = true})
    gtable.crush(
        ret._private, {
            horizontal = true,
            stretch_first = true,
            children = {},
        })
    gtable.crush(ret, compactor, true)
    return ret
end

setmetatable(compactor, {__call = function (self, ...) return compactor.new(...) end})

return compactor

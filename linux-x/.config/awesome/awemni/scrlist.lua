--- Vertically/horizontally scrolling list widgets.

local cairo = require("lgi").cairo
local base = require("wibox.widget.base")
local gtable = require("gears.table")

local scrlist = {}
local huge_size = 100000000

local alignment_to_number = {
    ["start"] = 1,
    ["end"] = -1,
    ["middle"] = 0,
}

local is_gravity_vertical = {
    ["top"] = true, ["bottom"] = true
}

local function array_length(a)
    local mt = getmetatable(a)
    if mt and mt.__len then return mt.__len(a) end
    return #a
end

function scrlist:compute_children_sizes(available_size, compute_child_size)
    local prev_sizes = {}
    local next_sizes = {}
    local anchor_index = self._private.anchor_index
    local alignment = alignment_to_number[self._private.alignment] or 1
    local children_count = array_length(self._private.children)
    local anchor_size = compute_child_size(self._private.children[anchor_index], available_size)
    local prev_available_size, next_available_size
    if alignment == -1 then
        prev_available_size = available_size - anchor_size
        next_available_size = 0
    elseif alignment == 1 then
        prev_available_size = 0
        next_available_size = available_size - anchor_size
    else
        prev_available_size = math.floor((available_size - anchor_size) / 2)
        next_available_size = available_size - anchor_size - prev_available_size
    end
    local total_size = anchor_size
    local finished = false
    while not finished do
        repeat
            local prev_size, next_size
            if prev_available_size > 0 then
                if anchor_index - #prev_sizes <= 1 then
                    next_available_size = next_available_size + prev_available_size
                    prev_available_size = 0
                    break
                else
                    prev_size = compute_child_size(self._private.children[anchor_index - #prev_sizes - 1], total_size)
                end
            end
            if next_available_size > 0 then
                if anchor_index + #next_sizes >= children_count then
                    if anchor_index - #prev_sizes > 1 then
                        prev_available_size = prev_available_size + next_available_size
                        next_available_size = 0
                        break
                    else
                        finished = true
                        break
                    end
                else
                    next_size = compute_child_size(self._private.children[anchor_index + #next_sizes + 1], total_size)
                end
            end
            if not prev_size and not next_size then
                finished = true
                break
            end
            if prev_size and prev_size > prev_available_size and
                next_size and next_size <= next_available_size then
                prev_size = nil
            end
            if next_size and next_size > next_available_size and
                prev_size and prev_size <= prev_available_size then
                next_size = nil
            end
            if prev_size then
                prev_available_size = prev_available_size - prev_size
                total_size = total_size + prev_size
                prev_sizes[#prev_sizes + 1] = prev_size
            else
                next_available_size = next_available_size - next_size
                total_size = total_size + next_size
                next_sizes[#next_sizes + 1] = next_size
            end
        until true
    end
    return anchor_size, prev_sizes, next_sizes, prev_available_size, next_available_size, total_size
end

function scrlist:get_height_measurement_function(context, content_width)
    return function (widget, available_height)
        local _w, h = base.fit_widget(self, context, widget, content_width, huge_size)
        if self._private.squeeze_overflow_child and h > available_height then
            _w, h = base.fit_widget(self, context, widget, content_width, available_height)
        end
        return h
    end
end

function scrlist:get_width_measurement_function(context, content_height)
    return function (widget, available_width)
        local w, _h = base.fit_widget(self, context, widget, huge_size, content_height)
        if self._private.squeeze_overflow_child and w > available_width then
            w, _h = base.fit_widget(self, context, widget, available_width, content_height)
        end
        return w
    end
end

function scrlist:layout(context, width, height)
    if width <= 0 or height <= 0 then return {} end
    local size = array_length(self._private.children)
    if size == 0 then return {} end
    local ret = {}
    if is_gravity_vertical[self._private.gravity] then
        local content_width = math.max(width - self._private.scrollbar_size, 0)
        local anchor_height, prev_heights, next_heights, prev_adjustment, next_adjustment
        while true do
            anchor_height, prev_heights, next_heights, prev_adjustment, next_adjustment =
                self:compute_children_sizes(
                    height, self:get_height_measurement_function(context, content_width))
            self._private.start_index = self._private.anchor_index - #prev_heights
            self._private.end_index = self._private.anchor_index + #next_heights
            if next_adjustment < 0 and #next_heights > 0 then
                self._private.end_index = self._private.end_index - 1
            end
            if prev_adjustment < 0 and #prev_heights > 0 then
                self._private.start_index = self._private.start_index + 1
            end
            local finished = false
            if self._private.view_index then
                if self._private.view_index < self._private.start_index then
                    self._private.anchor_index = self._private.view_index
                    self._private.alignment = "start"
                elseif self._private.view_index > self._private.end_index then
                    self._private.anchor_index = self._private.view_index
                    self._private.alignment = "end"
                else
                    finished = true
                end
            else
                finished = true
            end
            if finished and
                self._private.start_index == 1 and
                self._private.end_index == size and
                self._private.scrollbar_autohide and
                content_width < width then
                content_width = width
                finished = false
            end
            if finished then break end
        end
        local gravity = self._private.gravity
        local current_y = prev_adjustment
        for i = #prev_heights, 1, -1 do
            table.insert(ret, base.place_widget_at(
                             self._private.children[self._private.anchor_index - i],
                             0, (gravity == "top" and
                                 current_y or height - current_y - prev_heights[i]),
                             content_width, prev_heights[i]))
            current_y = current_y + prev_heights[i]
        end
        table.insert(ret, base.place_widget_at(
                         self._private.children[self._private.anchor_index],
                         0, (gravity == "top" and
                             current_y or height - current_y - anchor_height),
                         content_width, anchor_height))
        current_y = current_y + anchor_height
        for i = 1, #next_heights do
            table.insert(ret, base.place_widget_at(
                             self._private.children[self._private.anchor_index + i],
                             0, (gravity == "top" and
                                 current_y or height - current_y - next_heights[i]),
                             content_width, next_heights[i]))
            current_y = current_y + next_heights[i]
        end
    else
        local content_height = math.max(height - self._private.scrollbar_size, 0)
        local anchor_width, prev_widths, next_widths, prev_adjustment, next_adjustment
        while true do
            anchor_width, prev_widths, next_widths, prev_adjustment, next_adjustment =
                self:compute_children_sizes(
                    width, self:get_width_measurement_function(context, content_height))
            self._private.start_index = self._private.anchor_index - #prev_widths
            self._private.end_index = self._private.anchor_index + #next_widths
            if next_adjustment < 0 and #next_widths > 0 then
                self._private.end_index = self._private.end_index - 1
            end
            if prev_adjustment < 0 and #prev_widths > 0 then
                self._private.start_index = self._private.start_index + 1
            end
            local finished = false
            if self._private.view_index then
                if self._private.view_index < self._private.start_index then
                    self._private.anchor_index = self._private.view_index
                    self._private.alignment = "start"
                elseif self._private.view_index > self._private.end_index then
                    self._private.anchor_index = self._private.view_index
                    self._private.alignment = "end"
                else
                    finished = true
                end
            else
                finished = true
            end
            if finished and
                self._private.start_index == 1 and
                self._private.end_index == size and
                self._private.scrollbar_autohide and
                content_height < height then
                content_height = height
                finished = false
            end
            if finished then break end
        end
        local gravity = self._private.gravity
        local current_x = prev_adjustment
        for i = #prev_widths, 1, -1 do
            table.insert(ret, base.place_widget_at(
                             self._private.children[self._private.anchor_index - i],
                             (gravity == "left" and
                              current_x or width - current_x - prev_widths[i]), 0,
                             prev_widths[i], content_height))
            current_x = current_x + prev_widths[i]
        end
        table.insert(ret, base.place_widget_at(
                         self._private.children[self._private.anchor_index],
                         (gravity == "left" and
                          current_x or width - current_x - anchor_width), 0,
                         anchor_width, content_height))
        current_x = current_x + anchor_width
        for i = 1, #next_widths do
            table.insert(ret, base.place_widget_at(
                             self._private.children[self._private.anchor_index + i],
                             (gravity == "left" and
                              current_x or width - current_x - next_widths[i]), 0,
                             next_widths[i], content_height))
            current_x = current_x + next_widths[i]
        end
    end
    return ret
end

function scrlist:fit(context, width, height)
    if width <= 0 or height <= 0 or array_length(self._private.children) == 0 then return 0, 0 end
    if is_gravity_vertical[self._private.gravity] then
        local content_width = math.max(width - self._private.scrollbar_size, 0)
        local _, _, _, _, _, total_height = self:compute_children_sizes(
            height, self:get_height_measurement_function(context, content_width))
        return width, total_height
    else
        local content_height = math.max(height - self._private.scrollbar_size, 0)
        local _, _, _, _, _, total_width = self:compute_children_sizes(
            width, self:get_width_measurement_function(context, content_height))
        return total_width, height
    end
end

local function default_scrollbar(_context, cr, width, height, size, start_index, end_index)
    local s = cr:get_source()
    local _, r, g, b, a = s:get_rgba()
    cr:set_source_rgba(r, g, b, a / 8)
    cr:rectangle(0, 0, width, height)
    cr:fill()

    local y_start = height / size * (start_index - 1)
    local y_end = height / size * end_index

    cr:set_source(s)
    cr:rectangle(0, y_start, width, y_end - y_start)
    cr:fill()
end

function scrlist:draw(context, cr, width, height)
    if width <= 0 or height <= 0 or
        not self._private.start_index or
        not self._private.end_index or
        self._private.scrollbar_size == 0 then
        return
    end
    local effective_count = math.max(self._private.extended_count or 0, array_length(self._private.children))
    if self._private.start_index == 1 and
        self._private.end_index == effective_count and
        self._private.scrollbar_autohide then
        return
    end
    if self._private.transform_before_scrollbar ~= false then
        local m = cairo.Matrix()
        if self._private.gravity == "top" then
            m:init(1, 0, 0, 1, width - self._private.scrollbar_size, 0)
        elseif self._private.gravity == "bottom" then
            m:init(1, 0, 0, -1, width - self._private.scrollbar_size, height)
        elseif self._private.gravity == "left" then
            m:init(1, 0, 0, 1, 0, height - self._private.scrollbar_size)
        elseif self._private.gravity == "right" then
            m:init(1, 0, 0, -1, width, height - self._private.scrollbar_size)
        else
            return
        end
        cr:transform(m)
    end
    local scrollbar = (self._private.scrollbar or default_scrollbar)
    scrollbar(context, cr, self._private.scrollbar_size, height,
              effective_count, self._private.start_index, self._private.end_index)
end

function scrlist:get_children()
    return self._private.children
end

function scrlist:set_children(children)
    self._private.children = children or {}
    self:emit_signal("property::children")
end

function scrlist:reset()
    self.children = {}
    self.extended_count = nil
    self.view_index = nil
end

function scrlist:get_anchor_index()
    return self._private.anchor_index
end

function scrlist:set_anchor_index(value)
    value = math.max(1, math.min(value or 1, array_length(self._private.children)))
    if self._private.anchor_index ~= value then
        self._private.anchor_index = value
        self._private.start_index = nil
        self._private.end_index = nil
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:get_view_index()
    return self._private.view_index
end

function scrlist:set_view_index(value)
    value = value and math.max(1, math.min(value or 1, array_length(self._private.children)))
    if self._private.view_index ~= value then
        self._private.view_index = value
        if value then
            if self._private.start_index and self._private.start_index > value then
                self.anchor_index = value
                self.alignment = "start"
            elseif self._private.end_index and self._private.end_index < value then
                self.anchor_index = value
                self.alignment = "end"
            end
        end
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:get_start_index()
    return self._private.start_index
end

function scrlist:get_end_index()
    return self._private.end_index
end

function scrlist:set_gravity(value)
    value = value or "top"
    if self._private.gravity ~= value then
        self._private.gravity = value
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:set_alignment(value)
    value = value or "start"
    if self._private.alignment ~= value then
        self._private.alignment = value
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:set_scrollbar(scrollbar)
    self._private.scrollbar = scrollbar
    self:emit_signal("widget::redraw_needed")
end

function scrlist:set_scrollbar_size(scrollbar_size)
    if self._private.scrollbar_size ~= scrollbar_size then
        self._private.scrollbar_size = scrollbar_size
        self:emit_signal("widget::layout_changed")
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:set_extended_count(count)
    if self._private.extended_count ~= count then
        self._private.extended_count = count
        self:emit_signal("widget::redraw_needed")
    end
end

function scrlist:show_prev()
    if self._private.start_index then
        self.anchor_index = self._private.start_index - 1
        self.alignment = "start"
    end
end

function scrlist:show_next()
    if self._private.end_index then
        self.anchor_index = self._private.end_index + 1
        self.alignment = "end"
    end
end

function scrlist:show_prev_page(half)
    if self._private.start_index then
        self.anchor_index = self._private.start_index - 1
        self.alignment = half and "center" or "end"
    end
end

function scrlist:show_next_page(half)
    if self._private.end_index then
        self.anchor_index = self._private.end_index + 1
        self.alignment = half and "center" or "start"
    end
end

local function new(_args)
    local ret = base.make_widget(nil, nil, {enable_properties = true})

    gtable.crush(ret, scrlist, true)

    ret._private.children = {}
    ret._private.start_index = nil
    ret._private.end_index = nil
    ret._private.anchor_index = 1
    ret._private.view_index = nil
    ret._private.extended_count = nil
    ret._private.gravity = "top"
    ret._private.alignment = "start"
    ret._private.transform_before_scrollbar = nil
    ret._private.scrollbar_size = 4
    ret._private.scrollbar_autohide = true

    ret:connect_signal(
        "property::children", function (self)
            self._private.start_index = nil
            self._private.end_index = nil
            local len = array_length(self._private.children)
            self._private.anchor_index = math.max(1, math.min(self._private.anchor_index, len))
            if self._private.view_index then
                self._private.view_index = math.max(1, math.min(self._private.view_index, len))
            end
            self:emit_signal("widget::layout_changed")
            self:emit_signal("widget::redraw_needed")
        end)
    return ret
end

for _, g in ipairs{"top", "bottom", "left", "right"} do
    scrlist[g] = function ()
        local ret = new()
        ret._private.gravity = g
        return ret
    end
end

return scrlist

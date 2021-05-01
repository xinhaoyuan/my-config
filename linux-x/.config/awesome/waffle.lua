local capi = {
    mouse = mouse,
    screen = screen,
    awesome = awesome,
    client = client,
    drawable = drawable,
}
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local gtimer = require("gears.timer")
local gtable = require("gears.table")
local lgi = require("lgi")
local dpi = require("beautiful.xresources").apply_dpi
local af = require("my-autofocus")

-- A waffle view is a table with the following elements
--   .widget -- the entry widget
--   .key_handler (optional) -- the key handling function. It returns a boolean if the key event is captured.

local waffle = {
    -- TODO: Gravity is deprecated.
    gravity_ = "southwest",
}

local PlacementLayout = {mt = {}}

function PlacementLayout:fit(context, width, height)
    return width, height
end

function PlacementLayout:layout(context, width, height)
    if self._private.widget == nil then
        return
    end

    local w, h = wibox.widget.base.fit_widget(self, context, self._private.widget, width, height)
    local place_func = self._private.place_func or self.default_place_func
    local x, y = place_func(self, context, width, height, w, h)
    if self._private.shape_x ~= x or self._private.shape_y ~= y or self._private.shape_w ~= w or self._private.shape_h ~= h then
        self._private.shape_x = x
        self._private.shape_y = y
        self._private.shape_w = w
        self._private.shape_h = h
        context["wibox"].shape = function(cr, width, height)
            cr:rectangle(x, y, w, h)
        end
    end
    return { wibox.widget.base.place_widget_at(self._private.widget, x, y, w, h) }
end

function PlacementLayout:default_place_func(context, parent_w, parent_h, child_w, child_h)
    local drawable = context["wibox"]
    local geo = { x = drawable.anchor and (drawable.anchor.x - child_w / 2) or 0,
                  y = drawable.anchor and (drawable.anchor.y - child_h / 2) or 0,
                  width = child_w, height = child_h }
    local obj = {
        geometry = function(_, new_geo)
            if new_geo then
                geo.x = new_geo.x
                geo.y = new_geo.y
            end
            return geo
        end
    }
    local dgeo = drawable:geometry()
    awful.placement.no_offscreen(
        obj,
        {screen = drawable.screen}
    )
    return geo.x - dgeo.x, geo.y - dgeo.y
end

PlacementLayout.set_widget = wibox.widget.base.set_widget_common

function PlacementLayout:get_widget()
    return self._private.widget
end

function PlacementLayout:get_children()
    return {self._private.widget}
end

function PlacementLayout:set_children(children)
    self:set_widget(children[1])
end

function PlacementLayout:new(widget)
    local ret = wibox.widget.base.make_widget(nil, nil, {enable_properties = true})

    gtable.crush(ret, self, true)

    ret:set_widget(widget)

    return ret
end

function PlacementLayout.mt.__call(self, ...)
    return self:new(...)
end

setmetatable(PlacementLayout, PlacementLayout.mt)

waffle.widget_container = wibox.widget {
    widget = PlacementLayout,
}
-- waffle.widget_container:connect_signal(
--     "button::press",
--     function (_, x, y, button, _, info)
--         local f = info.drawable:find_widgets(x, y)
--         if #f == 1 then
--             -- Only happens only if clicking the empty area
--             waffle:hide()
--         end
-- end)

function waffle:update_layout(screen)
    screen = screen or (self.wibox_ and self.wibox_.screen)
    if screen then
        if beautiful.waffle_use_entire_screen then
            self.wibox_:geometry({
                    x = screen.geometry.x,
                    y = screen.geometry.y,
                    width = screen.geometry.width,
                    height = screen.geometry.height,
            })
        else
            self.wibox_:geometry({
                    x = screen.workarea.x,
                    y = screen.workarea.y,
                    width = screen.workarea.width,
                    height = screen.workarea.height,
            })
        end
    end

    if self.wibox_ and self.wibox_.widget == nil then
        self.wibox_.widget = self.widget_container
    end
end

function waffle:set_gravity(gravity)
    if self.gravity_ ~= gravity then
        self.gravity_ = gravity
        self:update_layout()
    end
end

function waffle:set_view(view, call_open)
    if self.view_ and self.view_.on_close then
        self.view_:on_close()
    end
    self.view_ = view
    if view then
        if call_open and view.on_open then
            view:on_open(self.wibox_.screen)
        end
        self.widget_container.widget = view.widget
    end
end

function waffle:clear_stack()
    if self.stack_ then
        for _, v in ipairs(self.stack_) do
            if v.on_close then
                v:on_close()
            end
        end
    end
    self.stack_ = nil
end

function waffle:is_in_view(view)
    return self.view_ == view
end

function waffle:get_waffle_wibox(screen)
    if self.screen_wibox_ == nil then
        self.screen_wibox_ = {}
    end
    local screen_wibox = self.screen_wibox_[screen]
    if screen_wibox == nil or
        screen_wibox.x ~= screen.geometry.x or
        screen_wibox.y ~= screen.geometry.y or
        screen_wibox.width ~= screen.geometry.width or
        screen_wibox.height ~= screen.geometry.height
    then
        screen_wibox = wibox{
            screen = screen,
            x = screen.geometry.x,
            y = screen.geometry.y,
            width = screen.geometry.width,
            height = screen.geometry.height,
            bg = "#00000000",
            opacity = 1,
            ontop = true,
            type = "dock",
            visible = true,
            input_passthrough = true,
        }

        local this = self
        screen_wibox:connect_signal(
            "mouse::enter",
            function ()
                if this.autohide_ and this.autohide_timer_.started then
                    this.autohide_timer_:stop()
                end
            end)
        screen_wibox:connect_signal(
            "mouse::leave",
            function ()
                if this.autohide_ and not this.autohide_timer_.started then
                    this.autohide_timer_:start()
                end
            end)
        self.screen_wibox_[screen] = screen_wibox
    end
    return screen_wibox
end

capi.screen.connect_signal(
    "removed",
    function (s)
        local screen_wibox = waffle.screen_wibox_[s]
        if screen_wibox ~= nil then
            print("Screen "..tostring(s).." is removed. Removing its waffle wibox.")
            screen_wibox:remove()
            waffle.screen_wibox_[s] = nil
        end
    end
)

local function on_root_button_press(x, y, details, state)
    waffle:hide()
end

local function on_client_button_press(c, x, y, details, state)
    waffle:hide()
end

local function on_drawable_button_press(d, x, y, details, state)
    if d ~= waffle.wibox_.drawable and not waffle.to_be_activated then
        waffle:hide()
    end
end

function waffle:connect_button_signals()
    capi.awesome.connect_signal("root_button::press", on_root_button_press)
    capi.client.connect_signal("button::press", on_client_button_press)
    capi.drawable.connect_signal("button::press", on_drawable_button_press)
end

function waffle:disconnect_button_signals()
    capi.awesome.disconnect_signal("root_button::press", on_root_button_press)
    capi.client.disconnect_signal("button::press", on_client_button_press)
    capi.drawable.disconnect_signal("button::press", on_drawable_button_press)
end

-- args.mode in {"push", "set", "set_top"}
function waffle:show(view, args)
    args = args or {}
    local mode = args.mode or "set"
    local screen = args.screen or awful.screen.focused()

    view = view or self.root_view_
    if self.wibox_ ~= nil and self.wibox_.screen ~= screen then
        self:hide()
    end
    if self.wibox_ == nil then
        if not self.autohide_ then af.manage_focus(screen) end
        self.focused_client = capi.client.focus
        capi.client.focus = nil
        self.wibox_ = self:get_waffle_wibox(screen)
    end
    self.wibox_.widget = self.widget_container

    if mode == "push" then
        self.stack_ = self.stack_ or {}
        table.insert(self.stack_, self.view_)
    end

    self:set_view(view, mode ~= "set_top")

    if mode == "set" then
        self:clear_stack()
    end

    if self.wibox_.input_passthrough then
        self:connect_button_signals()
        self.wibox_.input_passthrough = false
    end

    if type(args.anchor) == "table" then
        self.wibox_.anchor = args.anchor
    elseif args.anchor == "screen" or capi.mouse.screen ~= screen then
        local avail_area = screen.workarea
        self.wibox_.anchor = { x = avail_area.x + avail_area.width / 2,
                               y = avail_area.y + avail_area.height / 2 }
    elseif args.anchor == "mouse" then
        local coords = capi.mouse.coords()
        self.wibox_.anchor = coords
    end
    if self.keygrabber_ ~= nil then
        awful.keygrabber.stop(self.keygrabber_)
        self.keygrabber_ = nil
    end
    if self.autohide_ then
        self.autohide_timer_:again()
    else
        self.keygrabber_ = awful.keygrabber.run(
            function (mod, key, event)
                if #key == 1 then
                    key = key:lower()
                end
                if self.view_.key_handler and self.view_.key_handler(mod, key, event) then
                    -- pass
                elseif key == "Escape" then
                    if event == "press" then
                        self:hide()
                    end
                elseif key == "BackSpace" then
                    if event == "press" then
                        self:go_back()
                    end
                end
            end
        )
    end

    self.to_be_activated = true
    local self_ = self
    gtimer.delayed_call(function () self_.to_be_activated = false end)
end

function waffle:go_back()
    local headpos = self.stack_ and #self.stack_ or 0
    if headpos >= 1 then
        local last = self.stack_[headpos]
        table.remove(self.stack_, headpos)
        self:show(last, { mode = "set_top" })
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
        self.focused_client = nil
        if not self.autohide_ then
            af.unmanage_focus(self.wibox_.screen)
        end
        self.wibox_.input_passthrough = true
        self.wibox_.widget = nil
        self.wibox_ = nil
    end

    self:set_view(nil)
    self:clear_stack()
    self.autohide_ = false
    self:disconnect_button_signals()
end

waffle.autohide_lock_ = 0
waffle.autohide_ = false

function waffle:autohide(enabled, timeout_sec)
    if enabled == nil then
        return self.autohide_
    elseif self.view_ == nil then
        self.autohide_ = enabled
        if enabled then
            self.autohide_timer_ = gtimer{
                timeout = timeout_sec or 1,
                single_shot = true,
                callback = function ()
                    if waffle.autohide_lock_ <= 0 and waffle.autohide_ and
                        waffle.wibox_ and mouse.current_wibox ~= waffle.wibox_
                    then
                        waffle:hide()
                    end
                end,
            }
        end
    end
end

function waffle:autohide_lock_acquire()
    self.autohide_lock_ = self.autohide_lock_ + 1
    if not self.autohide_ then
        return
    end
    if self.autohide_lock_ > 0 and self.autohide_timer_.started then
        self.autohide_timer_:stop()
    end
end

function waffle:autohide_lock_release()
    self.autohide_lock_ = self.autohide_lock_ - 1
    if not self.autohide_ then
        return
    end
    if self.autohide_lock_ <= 0 and not self.autohide_timer_.started then
        self.autohide_timer_:start()
    end
end

function waffle:set_root_view(v)
    self.root_view_ = v
end

return waffle

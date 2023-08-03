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

-- A waffle view is a widget with the following optional handlers which returns a boolean if the event is handled.
--   :handle_key(mods, key, event)
--   :handle_back()
--   :handle_open(screen, is_new)
--   :handle_close()

local waffle = {
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
    local wb = context["wibox"]
    if self._private.apply_shape then
        if self._private.shape_x ~= x or
            self._private.shape_y ~= y or
            self._private.shape_w ~= w or
            self._private.shape_h ~= h then
            self._private.shape_x = x
            self._private.shape_y = y
            self._private.shape_w = w
            self._private.shape_h = h
            wb.shape = function(cr, width, height)
                cr:rectangle(x, y, w, h)
            end
        end
    else
        wb.shape = nil
    end
    return { wibox.widget.base.place_widget_at(self._private.widget, x, y, w, h) }
end

function PlacementLayout:default_place_func(context, parent_w, parent_h, child_w, child_h)
    local drawable = context["wibox"]
    local geo = { x = drawable.anchor and math.floor(drawable.anchor.x - child_w / 2) or 0,
                  y = drawable.anchor and math.floor(drawable.anchor.y - child_h / 2) or 0,
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

function PlacementLayout:set_apply_shape(v)
    if self._private.apply_shape ~= v then
        self._private.apply_shape = v
        if not v then
            self._private.shape_x = nil
            self._private.shape_y = nil
            self._private.shape_w = nil
            self._private.shape_h = nil
        end
        self:emit_signal("widget::layout_changed")
    end
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
    {
        widget = wibox.container.background,
    },
    widget = PlacementLayout,
}
waffle.widget_container:connect_signal(
    "button::press",
    function (_, _x, _y, button, _mod, _info)
        if waffle.mouse_buttons_ == nil then
            waffle.mouse_buttons_ = {}
        end
        waffle.mouse_buttons_[button] = true
    end)
waffle.widget_container:connect_signal(
    "button::release",
    function (_, x, y, button, _mod, info)
        if waffle.mouse_buttons_ == nil or not waffle.mouse_buttons_[button] then return end
        local f = info.drawable:find_widgets(x, y)
        if #f == 1 then
            -- Only happens only if clicking the empty area
            if button == 1 then
                waffle:hide()
            elseif button == 3 then
                waffle:go_back()
            end
        end
    end)
waffle.widget_container:connect_signal(
    "mouse::leave",
    function (_self, _info)
        -- Some time false positive can happen.
        local c = capi.mouse.coords()
        local g = _info.drawable.wibox:geometry()
        if g.x <= c.x and c.x < g.x + g.width and g.y < c.y and c.y < g.y + g.height then return end

        if waffle.mouse_entered_ and not waffle:autohide() then
            waffle:hide()
        end
    end)
waffle.widget_container:connect_signal(
    "mouse::enter",
    function (_self, _info)
        waffle.mouse_entered_ = true
    end)

waffle.widget_container.widget:connect_signal(
    "mouse::leave",
    function ()
        waffle:autohide_delayed_check(true)
    end)

function waffle:set_view(view, is_new_view)
    if self.view_ and self.view_.handle_close then
        self.view_:handle_close()
    end
    self.view_ = view
    if view then
        if view.handle_open then
            view:handle_open(self.wibox_.screen, is_new_view)
        end
        self.widget_container.widget.widget = view
    end
end

function waffle:clear_stack()
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
    local screen_geo = screen.geometry
    if screen_wibox == nil or
        screen_wibox.x ~= screen_geo.x or
        screen_wibox.y ~= screen_geo.y or
        screen_wibox.width ~= screen_geo.width or
        screen_wibox.height ~= screen_geo.height
    then
        screen_wibox = wibox{
            screen = screen,
            x = screen_geo.x,
            y = screen_geo.y,
            width = screen_geo.width,
            height = screen_geo.height,
            bg = "#00000000",
            opacity = 1,
            ontop = true,
            type = "dock",
            visible = false,
            input_passthrough = true,
        }

        local this = self
        self.screen_wibox_[screen] = screen_wibox
    end
    return screen_wibox
end

capi.screen.connect_signal(
    "removed",
    function (s)
        if s == nil then
            print("!!! screen::removed with nil screen:", debug.traceback())
            return
        end
        local screen_wibox = waffle.screen_wibox_[s]
        if screen_wibox ~= nil then
            -- I think a mouse leave would emit on the wibox, causing
            -- it to hide, but just in case it didn't...
            if screen_wibox == waffle.wibox_ then
                print("Hide waffle due to screen removal.")
                waffle:hide()
            end
            print("Screen "..tostring(s).." is removed. Removing its waffle wibox.")
            screen_wibox.visible = false
            waffle.screen_wibox_[s] = nil
        end
    end
)

-- args.mode in {"push", "set", "pop"}
function waffle:show(view, args)
    args = args or {}
    local mode = args.mode or "set"
    local screen = args.screen or awful.screen.focused()

    view = view or self.root_view_
    if self.wibox_ ~= nil and self.wibox_.screen ~= screen then
        self:hide()
    end

    if (self.wibox_ == nil or self.autohide_) and mode == "set" and not args.autohide then
        self.focused_client = capi.client.focus
        af.manage_focus(screen)
        capi.client.focus = nil
    end

    if mode == "set" then
        if self.autohide_timer_ == nil then
            local this = self
            self.autohide_timer_ = gtimer{
                single_shot = true,
                callback = function ()
                    if this.autohide_ then
                        if this.wibox_ and capi.mouse.current_wibox == this.wibox_ and #capi.mouse.current_widgets > 1 then
                            return
                        end
                        if self.autohide_locking_callback_ and self.autohide_locking_callback_() then
                            return
                        end
                        this:hide()
                    end
                end,
            }
        end
        if self.autohide_timer_.started then
            self.autohide_timer_:stop()
        end
        if args.autohide then
            self.autohide_ = true
            self.autohide_locking_callback_ = args.autohide_locking_callback
            self.autohide_timer_.timeout = tonumber(args.autohide) or 1
            self.widget_container.apply_shape = true
        else
            self.autohide_ = false
            self.widget_container.apply_shape = false
        end
    end

    if self.wibox_ == nil then
        self.wibox_ = self:get_waffle_wibox(screen)
    end
    self.wibox_.widget = self.widget_container

    if mode == "push" then
        self.stack_ = self.stack_ or {}
        table.insert(self.stack_, self.view_)
    end

    self:set_view(view, mode ~= "pop")

    if mode == "set" then
        self:clear_stack()
    end

    if self.stack_ == nil or #self.stack_ == 0 then
        capi.awesome.emit_signal("waffle.show")
    end

    if self.wibox_.input_passthrough then
        self.wibox_.visible = true
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
                if self.view_.handle_key and self.view_:handle_key(mod, key, event) then
                    -- pass
                elseif key == "Escape" then
                    if event == "press" then
                        self:hide()
                        local tmp_grabber
                        tmp_grabber = awful.keygrabber.run(
                            function (mod, key, event)
                                if key == "Escape" and event == "release" then
                                    awful.keygrabber.stop(tmp_grabber)
                                end
                            end)
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
    if self.view_ and self.view_.handle_back and self.view_:handle_back() then
        return
    end
    local headpos = self.stack_ and #self.stack_ or 0
    if headpos >= 1 then
        local last = self.stack_[headpos]
        table.remove(self.stack_, headpos)
        self:show(last, { mode = "pop" })
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
        self.wibox_.visible = false
        self.wibox_.widget = nil
        self.wibox_ = nil
    end

    self:set_view(nil)
    self:clear_stack()
    self.autohide_ = false
    self.autohide_locking_callback_ = nil
    self.mouse_entered_ = false
    self.mouse_buttons_ = nil
    capi.awesome.emit_signal("waffle.hide")
end

function waffle:autohide_delayed_check(reset)
    if self.autohide_ then
        if self.autohide_timer_.started then
            if reset then
                self.autohide_timer_:stop()
            else
                return
            end
        end
        self.autohide_timer_:start()
    end
end

function waffle:set_root_view(v)
    self.root_view_ = v
end

function waffle:get_screen()
    if self.view_ == nil then
        return nil
    else
        return self.wibox_.screen
    end
end

function waffle:autohide()
    return self.autohide_
end

return waffle

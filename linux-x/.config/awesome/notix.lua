-- Notification center.
-- Only works with awesome-git.

local capi = {
    awesome = awesome,
}
local gtimer = require("gears.timer")
local gcolor = require("gears.color")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local fixed_margin = require("fixed_margin")
local masked_imagebox = require("masked_imagebox")
local ocontainer = require("onion.container")
local opicker = require("onion.picker")
local fallback = require("fallback")
local naughty = require("naughty")
local icons = require("icons")
local scroller = require("scroller")

local config = {
    initialize_naughty = true,
}

local notif_id_counter = 0
local notif_counter = 0
local notif_widgets = {}
local notif_objects = {}

local remove_unpinned -- forward function desc

local notix_counter_header = wibox.widget.textbox("<span size='large'>Notifications</span>")
local notix_header_bar = wibox.widget{
    {
        {
            nil,
            notix_counter_header,
            {
                {
                    {
                        image = gcolor.recolor_image(icons.remove, beautiful.fg_normal),
                        widget = masked_imagebox,
                    },
                    forced_height = beautiful.icon_size,
                    forced_width = beautiful.icon_size,
                    margins = beautiful.sep_small_size / 2,
                    widget = wibox.container.margin,
                },
                halign = "right",
                widget = wibox.container.place,
            },
            expand = "outside",
            layout = wibox.layout.align.horizontal,
        },
        left = beautiful.sep_small_size,
        widget = wibox.container.margin,
    },
    fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
    bg_picker = opicker.beautiful{"bg_", opicker.highlighted_switcher},
    context_transformation = {highlighted = false},
    visible = false,
    widget = ocontainer,
}

notix_header_bar:connect_signal(
    "mouse::enter",
    function ()
        notix_header_bar.context_transformation = {highlighted = true}
    end
)
notix_header_bar:connect_signal(
    "mouse::leave",
    function ()
        notix_header_bar.context_transformation = {highlighted = false}
    end
)
notix_header_bar:connect_signal(
    "button::release",
    function ()
        remove_unpinned()
    end
)

local notix_pinned_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_reg_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_widget = wibox.widget{
    {
        notix_header_bar,
        notix_pinned_container,
        layout = wibox.layout.fixed.vertical,
    },
    {
        {
            notix_reg_container,
            gravity = "top",
            widget = scroller,
        },
        widget = wibox.container.constraint,
    },
    layout = wibox.layout.align.vertical,
}

local notix_counter_widget = wibox.widget{
    text = "0",
    widget = wibox.widget.textbox,
}

local function update_notif_counter(delta)
    notif_counter = notif_counter + delta
    if notif_counter <= 0 then
        notix_header_bar.visible = false
    elseif notif_counter <= delta then
        notix_header_bar.visible = true
    end
    notix_counter_widget.text = tostring(notif_counter)
    capi.awesome.emit_signal("notix::on_counter_change", notif_counter)
end

local function add_widget_to_container(widget, container)
    if widget.notif_container then
        widget.notif_container:remove_widgets(widget)
    end
    widget.notif_container = container
    container:add(widget)
    container:emit_signal("property::children")
end

capi.awesome.connect_signal(
    "notix::on_notification",
    function(notif)
        local notif_id = notif_id_counter
        notif_id_counter = notif_id + 1
        notif.notif_id = notif_id

        local notif_widget = config.create_notif_widget(notif)
        notif_widget.notif_id = notif_id

        notif_widgets[notif_id] = notif_widget
        notif_objects[notif_id] = notif

        update_notif_counter(1)
        add_widget_to_container(notif_widget, notix_reg_container)
    end
)

local remove_notification

function config.create_notif_widget(notif)
    notif:connect_signal("destroyed",
                         function (notif, reason)
                             -- Expired
                             if reason ~= 1 then
                                 notif._private.destroy_reason =
                                     notif._private.destroy_reason or reason
                                 remove_notification(notif)
                             end
    end)
    local action_container = wibox.widget{
        spacing = beautiful.sep_small_size,
        spacing_widget = beautiful.sep_widget,
        layout = wibox.layout.flex.horizontal,
    }
    for _, action in ipairs(notif.actions) do
        local callback = function ()
            action:invoke()
            if not notif.resident then
                remove_notification(notif)
            end
        end
        action_container:add(config.create_button(action.name, callback))
    end
    local content_widget = wibox.widget{
        {
            {
                {
                    {
                        {
                            notification = notif,
                            widget = naughty.widget.icon,
                        },
                        {
                            image = icons.notification,
                            widget = masked_imagebox,
                        },
                        widget = fallback,
                    },
                    valign = "center",
                    halign = "center",
                    widget = wibox.container.place,
                },
                width = beautiful.icon_size,
                strategy = "exact",
                widget = wibox.container.constraint,
            },
            right = beautiful.sep_small_size,
            widget = wibox.container.margin,
        },
        {
            {
                {
                    text = (notif.app_name and #notif.app_name > 0 and notif.app_name..":" or "")
                        ..notif.title,
                    widget = wibox.widget.textbox,
                },
                bottom = beautiful.sep_small_size,
                draw_empty = false,
                widget = wibox.container.margin,
            },
            {
                text = notif.message,
                font = beautiful.font_name_normal.." "..tostring(beautiful.font_size_small),
                widget = wibox.widget.textbox,
            },
            layout = wibox.layout.fixed.vertical,
        },
        layout = wibox.layout.fixed.horizontal,
    }
    content_widget:connect_signal(
        "button::release",
        function (_, _x, _y, button)
            -- if button == 1 then
            --     remove_notification(notif, 2)
            -- else
            if button == 2 then
                remove_notification(notif, 1)
            elseif button == 3 then
                if config.org_file_for_pin then
                    f = io.open(config.org_file_for_pin, "a")
                    if f then
                        f:write(string.format(
                                    "\n* TODO %s%s%s\n",
                                    notif.app_name and #notif.app_name > 0 and notif.app_name..": " or "",
                                    notif.title and #notif.title > 0 and notif.title.." - " or "",
                                    notif.message))
                        f:close()
                        require("orgenda").schedule_reset()
                    end
                    remove_notification(notif, 1)
                elseif notif_widgets[notif.notif_id] == notix_reg_container then
                    add_widget_to_container(notif_widgets[notif.notif_id], notix_pinned_container)
                else
                    add_widget_to_container(notif_widgets[notif.notif_id], notix_reg_container)
                end
            end
        end
    )

    local ret = wibox.widget{
        {
            content_widget,
            action_container,
            layout = wibox.layout.fixed.vertical,
        },
        fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
        bg_picker = opicker.beautiful{"bg_", opicker.highlighted_switcher},
        context_transformation = {highlighted = false},
        widget = ocontainer,
    }
    ret.notif = notif

    function ret:execute()
        remove_notification(notif, 2)
    end

    function ret:key_handler(mods, key, event)
        if event ~= "press" then return false end
        local c = tonumber(key)
        if c == nil then return false end
        if notif.actions[c] then
            notif.actions[c]:invoke()
            if not notif.resident then
                remove_notification(notif)
            end
            return true
        end
        return false
    end

    function ret:set_focused(f)
        ret.context_transformation = {highlighted = f}
    end

    return ret
end

function config.create_button(name, callback)
    local widget = wibox.widget{
        {
            {
                text = name,
                font = beautiful.font_name_normal.." "..tostring(beautiful.font_size_small),
                align = "center",
                widget = wibox.widget.textbox,
            },
            margins = beautiful.sep_small_size,
            widget = wibox.container.margin,
        },
        fg_picker = opicker.beautiful{"fg_", opicker.highlighted_switcher},
        bg_picker = opicker.beautiful{"bg_", opicker.highlighted_switcher},
        context_transformation = {highlighted = nil},
        widget = ocontainer,
    }
    widget:connect_signal(
        "mouse::enter",
        function (w)
            w.context_transformation = {highlighted = true}
        end
    )
    widget:connect_signal(
        "mouse::leave",
        function (w)
            w.context_transformation = {highlighted = nil}
        end
    )
    widget:connect_signal(
        "button::release",
        callback
    )
    return widget
end

local function add_notification(notif)
    capi.awesome.emit_signal("notix::on_notification", notif)
end

remove_notification = function (notif, reason)
    if notif_widgets[notif.notif_id] == nil then
        return
    end
    local container = notif_widgets[notif.notif_id].notif_container
    container:remove_widgets(notif_widgets[notif.notif_id])
    notif_widgets[notif.notif_id] = nil
    container:emit_signal("property::children")

    -- This is a hack depending on that :destroy does not really call _private.destroy.
    -- Note that we won't call _private.destory twice thanks to the guard above.
    if notif._private.destroy_reason then
        notif.real_destroy(reason or notif._private.destroy_reason)
    else
        reason = reason or 2
        notif:destroy(reason)
        notif.real_destroy(reason)
    end
    notif_objects[notif.notif_id] = nil

    update_notif_counter(-1)
end

remove_unpinned = function()
    local removed_counter = 0
    for id, widget in pairs(notif_widgets) do
        if widget.notif_container == notix_reg_container then
            remove_notification(notif_objects[id], 1)
        end
    end
end

gtimer.delayed_call(
    function ()
        naughty.connect_signal(
            "new",
            function (notif, args)
                notif.real_destroy = args.destroy or function () end
                args.destroy = function () end
                if not args.skip_notix and (config.filter == nil or config.filter(notif)) then
                    add_notification(notif)
                end
            end
        )
    end
)

return {
    config = config,
    add_notification = add_notification,
    remove_notification = remove_notification,
    remove_unpinned = remove_unpinned,
    widget = notix_widget,
    regular_container = notix_reg_container,
    pinned_container = notix_pinned_container,
    counter_widget = notix_counter_widget,
}

local capi = {
    awesome = awesome,
}
local gtimer = require("gears.timer")
local wibox = require("wibox")
local beautiful = require("beautiful")

local config = {
    initialize_naughty = true,
}

local notif_id_counter = 0
local notif_widgets = {}
local notif_objects = {}

local notix_pinned_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_reg_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_widget = wibox.widget{
    notix_pinned_container,
    notix_reg_container,
    layout = wibox.layout.fixed.vertical,
}

local function add_widget_to_container(widget, container)
    if widget.notif_container then
        widget.notif_container:remove_widgets(widget)
    end
    widget.notif_container = container
    container:add(widget)
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

        add_widget_to_container(notif_widget, notix_reg_container)
    end
)

function config.create_notif_widget(notif)
    local action_container = wibox.widget{
        layout = wibox.layout.flex.horizontal,
    }
    for _, action in pairs(notif.actions) do
        local callback = function ()
            action:invoke()
            notif_widgets[notif.notif_id].notif_container:remove_widgets(
                notif_widgets[notif.notif_id])
        end
        action_container:add(config.create_notif_button(action.name, callback))
    end
    action_container:add(
        config.create_notif_button(
            "Ignore",
            function ()
                notif_widgets[notif.notif_id].notif_container:remove_widgets(
                    notif_widgets[notif.notif_id])
            end
    ))
    return wibox.widget{
        {
            {
                text = notif.appname,
                widget = wibox.widget.textbox,
            },
            margins = beautiful.sep_small_size,
            widget = wibox.container.margin,
        },
        {
            {
                text = notif.message,
                font = beautiful.fontname_normal.." "..tostring(beautiful.fontsize_small),
                widget = wibox.widget.textbox,
            },
            left = beautiful.sep_small_size,
            right = beautiful.sep_small_size,
            widget = wibox.container.margin,
        },
        action_container,
        layout = wibox.layout.fixed.vertical,
    }
end

function config.create_notif_button(name, callback)
    local widget = wibox.widget{
        {
            {
                {
                    text = name,
                    align = "center",
                    widget = wibox.widget.textbox,
                },
                margins = beautiful.sep_small_size,
                widget = wibox.container.margin,
            },
            fg = beautiful.fg_focus,
            bg = beautiful.bg_focus,
            widget = wibox.container.background,
        },
        margins = beautiful.sep_small_size,
        widget = wibox.container.margin,
    }
    widget:connect_signal(
        "button::release",
        function()
            print("action ", name)
            callback()
        end
    )
    return widget
end

local function add_notification(notif)
    capi.awesome.emit_signal("notix::on_notification", notif)
end

gtimer.delayed_call(
    function ()
        if not config.initialize_naughty then
            return
        end

        require("naughty").config.notify_callback = function (notif)
            add_notification(notif)
            return notif
        end
    end
)
return {
    config = config,
    add_notification = add_notification,
    widget = notix_widget,
}

local capi = {
    awesome = awesome,
}
local gtimer = require("gears.timer")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local fixed_margin = require("fixed_margin")
local naughty = require("naughty")

local config = {
    initialize_naughty = true,
}

local notif_id_counter = 0
local notif_counter = 0
local notif_widgets = {}
local notif_objects = {}

local notix_counter_textbox = wibox.widget.textbox()
local notix_header_bar = wibox.widget{
    {
        notix_counter_textbox,
        nil,
        nil,
        layout = wibox.layout.align.horizontal,
    },
    left = beautiful.sep_small_size,
    right = beautiful.sep_small_size,
    widget = wibox.container.margin,
    visible = false,
}
local notix_pinned_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_reg_container = wibox.widget{
    layout = wibox.layout.fixed.vertical,
}
local notix_widget = wibox.widget{
    notix_header_bar,
    notix_pinned_container,
    notix_reg_container,
    layout = wibox.layout.fixed.vertical,
}

local function update_notif_counter(delta)
    notif_counter = notif_counter + delta
    if notif_counter <= 0 then
        notix_header_bar.visible = false
    elseif notif_counter <= delta then
        notix_header_bar.visible = true
    end
    notix_counter_textbox.text = tostring(notif_counter).." notifications:"
end

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

        update_notif_counter(1)
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
        action_container:add(config.create_button(action.name, callback))
    end
    action_container:add(
        config.create_button(
            "Ignore",
            function ()
                notif_widgets[notif.notif_id].notif_container:remove_widgets(
                    notif_widgets[notif.notif_id])
                notif_widgets[notif.notif_id] = nil
                notif_objects[notif.notif_id] = nil
                update_notif_counter(-1)
            end
    ))
    return wibox.widget{
        {
            {
                {
                    {
                        {
                            image = notif.icon,
                            widget = wibox.widget.imagebox,
                        },
                        height = dpi(32),
                        width = dpi(32),
                        strategy = "max",
                        widget = wibox.container.constraint,
                    },
                    right = beautiful.sep_small_size,
                    draw_empty = false,
                    widget = wibox.container.margin,
                },
                {
                    text = notif.app_name,
                    widget = wibox.widget.textbox,
                },
                {
                    {
                        text = notif.title,
                        widget = wibox.widget.textbox,
                    },
                    left = beautiful.sep_small_size,
                    draw_empty = false,
                    widget = wibox.container.margin,
                },
                layout = wibox.layout.align.horizontal,
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

function config.create_button(name, callback)
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
        notix_header_bar.widget.third = config.create_button(
            "Ignore all", function ()
                notix_pinned_container:reset()
                notix_reg_container:reset()
                notif_widgets = {}
                notif_objects = {}
                update_notif_counter(-notif_counter)
            end
        )

        naughty.connect_signal(
            "new",
            function (notif)
                if config.filter == nil or config.filter(notif) then
                    add_notification(notif)
                end
            end
        )
    end
)

return {
    config = config,
    add_notification = add_notification,
    widget = notix_widget,
}

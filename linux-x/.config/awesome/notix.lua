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
local cbg = require("contextual_background")
local naughty = require("naughty")
local icons = require("icons")

local config = {
    initialize_naughty = true,
}

local notif_id_counter = 0
local notif_counter = 0
local notif_widgets = {}
local notif_objects = {}

local notix_counter_header = wibox.widget.textbox()
local notix_header_bar = wibox.widget{
    {
        notix_counter_header,
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

local notix_counter_number = wibox.widget{
    text = "0",
    widget = wibox.widget.textbox
}
local notix_counter_widget = wibox.widget{
    {
        {
            image = gcolor.recolor_image(icons.notification, beautiful.fg_normal),
            forced_height = beautiful.bar_icon_size,
            forced_width = beautiful.bar_icon_size,
            widget = masked_imagebox,
        },
        valign = "center",
        widget = wibox.container.place,
    },
    notix_counter_number,
    visible = false,
    layout = wibox.layout.fixed.horizontal,
}

local function update_notif_counter(delta)
    notif_counter = notif_counter + delta
    if notif_counter <= 0 then
        notix_header_bar.visible = false
        notix_counter_widget.visible = false
    elseif notif_counter <= delta then
        notix_header_bar.visible = true
        notix_counter_widget.visible = true
    end
    notix_counter_header.text = tostring(notif_counter).." notifications:"
    notix_counter_number.text = tostring(notif_counter)
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

local function remove_notification(notif)
    notif_widgets[notif.notif_id].notif_container:remove_widgets(
        notif_widgets[notif.notif_id])
    notif_widgets[notif.notif_id] = nil
    notif_objects[notif.notif_id] = nil
    update_notif_counter(-1)
end

function config.create_notif_widget(notif)
    local action_container = wibox.widget{
        spacing = beautiful.sep_small_size,
        spacing_widget = beautiful.sep_widget,
        layout = wibox.layout.flex.horizontal,
    }
    for _, action in pairs(notif.actions) do
        local callback = function ()
            action:invoke()
            remove_notification(notif)
        end
        action_container:add(config.create_button(action.name, callback))
    end
    local pin_button, unpin_button
    pin_button = config.create_button(
        config.org_file_for_pin and "Save" or "Pin",
        function ()
            if config.org_file_for_pin then
                f = io.open(config.org_file_for_pin, "a")
                if f then
                    f:write(string.format(
                                "\n* TODO %s%s%s\n",
                                notif.app_name and notif.app_name..": " or "",
                                notif.title and notif.title.." - " or "",
                                notif.message))
                    f:close()
                end
                remove_notification(notif)
            else
                add_widget_to_container(notif_widgets[notif.notif_id], notix_pinned_container)
                action_container:replace_widget(pin_button, unpin_button)
            end
        end
    )
    unpin_button = config.create_button(
        "Unpin",
        function ()
            add_widget_to_container(notif_widgets[notif.notif_id], notix_pinned_container)
            action_container:replace_widget(unpin_button, pin_button)
        end
    )
    action_container:add(pin_button)
    action_container:add(
        config.create_button(
            "Ignore",
            function ()
                remove_notification(notif)
            end
    ))
    return wibox.widget{
        {
            {
                {
                    {
                        {
                            notification = notif,
                            widget = naughty.widget.icon,
                        },
                        valign = "center",
                        widget = wibox.container.place,
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
                text = name,
                align = "center",
                widget = wibox.widget.textbox,
            },
            margins = beautiful.sep_small_size,
            widget = wibox.container.margin,
        },
        fg_function = {"fg_"},
        bg_function = {"bg_"},
        context_transform_function = {focus = false},
        widget = cbg,
    }
    widget:connect_signal(
        "mouse::enter",
        function ()
            widget:set_context_transform_function({focus = true})
        end
    )
    widget:connect_signal(
        "mouse::leave",
        function ()
            widget:set_context_transform_function({focus = false})
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

local function remove_unpinned()
    local removed_counter = 0
    for id, widget in pairs(notif_widgets) do
        if widget.notif_container == notix_reg_container then
            removed_counter = removed_counter + 1
            notif_widgets[id] = nil
            notif_objects[id] = nil
        end
    end
    notix_reg_container:reset()
    update_notif_counter(-removed_counter)
end

gtimer.delayed_call(
    function ()
        notix_header_bar.widget.third = config.create_button(
            config.org_file_for_pin and "Ignore all" or "Ignore unpinned", remove_unpinned)

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
    remove_notification = remove_notification,
    remove_unpinned = remove_unpinned,
    widget = notix_widget,
    counter_widget = notix_counter_widget,
}

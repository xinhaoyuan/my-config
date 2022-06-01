local capi = {
    awesome = awesome,
    screen = screen,
    mouse = mouse,
    client = client,
    root = root,
}
local gcolor = require("gears.color")
local icons = require("icons")
local beautiful = require("beautiful")
local awful = require("awful")
local shared = require("config.shared")
local dpi = require("beautiful.xresources").apply_dpi
local wibox  = require("wibox")
local masked_imagebox = require("masked_imagebox")
local fallback = require("fallback")
local ocontainer = require("onion.container")
local border = require("border-theme")
local fixed_margin = require("fixed_margin")
local fixed_place = require("fixed_place")
local fixed_align = require("fixed_align")
local matrix = require("gears.matrix")
local waffle = require("waffle")
local outlined_textbox = require("outlined_textbox")
local debug_container = require("debug_container")
local module = {}

local size_index = shared.size_index
local dual_size_index = shared.dual_size_index
local top_index = shared.top_index
local bottom_index = shared.bottom_index
local left_index = shared.left_index
local right_index = shared.right_index
local direction_index = shared.direction_index
local dual_direction_index = shared.dual_direction_index
local gravity_index = shared.gravity_index

local waffle_was_there = false
local function tasklist_item_button(w, c, b)
    if b == 1 then
        if capi.client.focus == c and not waffle_was_there then
            c.minimized = true
        else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() then
                awful.tag.viewonly(c:tags()[1])
            end
            -- This will also un-minimize
            -- the client, if needed
            capi.client.focus = c
            c:raise()
        end
    elseif b == 2 then
        c:kill()
    elseif b == 3 then
        local wb = capi.mouse.current_wibox
        local geos = capi.mouse.current_widget_geometries
        local wgeo
        for _, geo in ipairs(geos) do
            if geo.widget == w then
                wgeo = geo
                break
            end
        end
        if wgeo ~= nil then
            shared.waffle.show_client_waffle(c, { anchor = { x = wb.x + wgeo.x + wgeo.width / 2, y = wb.y + wgeo.y + wgeo.height / 2 } })
        else
            shared.waffle.show_client_waffle(c)
        end
    elseif b == 4 then
        if not c.maximized then
            shared.client.enlarge(c)
        end
    elseif b == 5 then
        if c.maximized then
            shared.client.shrink(c)
        end
    end
end

local function attach_tasklist_item_buttons(w, c)
    w.button_pressed = {}
    w:connect_signal(
        'button::press', function (w, x, y, button)
            waffle_was_there =
                shared.waffle_selected_client ~= nil and not waffle:autohide()
            w.button_pressed[button] = true
        end)
    w:connect_signal(
        'button::release', function (w, x, y, button)
            if w.button_pressed[button] then
                w.button_pressed[button] = false
                tasklist_item_button(w, c, button)
            end
        end)
    w:connect_signal(
        'mouse::enter', function (w, x, y, button)
            local wb = capi.mouse.current_wibox
            local geos = capi.mouse.current_widget_geometries
            local wgeo
            for _, geo in ipairs(geos or {}) do
                if geo.widget == w then
                    wgeo = geo
                    break
                end
            end
            if wgeo ~= nil then
                local waffle_scr = waffle:get_screen()
                if waffle:autohide() or waffle_scr == nil or waffle_scr ~= wb.screen then
                    local locking_callback = function ()
                        if capi.mouse.current_widgets then
                            for _, ow in ipairs(capi.mouse.current_widgets) do
                                if ow == w then return true end
                            end
                        end
                    end
                    shared.waffle.show_client_waffle(
                        c, {
                            anchor = {
                                x = wb.x + wgeo.x + wgeo.width / 2,
                                y = wb.y + wgeo.y + wgeo.height / 2
                            },
                            autohide = 0.5,
                            autohide_locking_callback = locking_callback,
                        })
                end
            end
        end)
    w:connect_signal(
        'mouse::leave', function (w, x, y, button)
            if w.button_pressed[1] then
                c:raise()
                c.maximized = false
                c.minimized = false
                local geo = c:geometry()
                mouse.coords({ x = geo.x + geo.width / 2, y = geo.y + geo.height / 2 })
                awful.mouse.client.move(c)
            elseif w.button_pressed[3] then
                c:raise()
                c.maximized = false
                c.minimized = false
                awful.mouse.client.resize(c, "bottom_right")
            end
            w.button_pressed = {}
            waffle:autohide_delayed_check(true)
        end)
end

capi.client.connect_signal("list", function() waffle:autohide_delayed_check() end)

local property_to_text = {
    {"sticky", "S"},
    {"above", "A"},
    {"ontop", "T"},
    {"floating", "F"},
    {"maximized", "M"},
    {"minimized", ""},
}

local function tasklist_update_function(widget, c, index, objects)
    local status_widget = widget:get_children_by_id("status_role")[1]
    local inline_status_widget = widget:get_children_by_id("inline_status_role")[1]
    local background_widget = widget:get_children_by_id("my_background_role")[1]
    local title_widget = widget:get_children_by_id("title_text_role")[1]
    local status_text = ""
    local prop = {}
    local forced_width
    local group_hidden = c.cgroup and c.cgroup.current_client ~= c
    local icon_only
    if not c.screen.client_compare then
        if c.cgroup == nil then icon_only = c.tasklist_icon_only
        else icon_only = group_hidden
        end
    end
    if group_hidden then
        if not icon_only then forced_width = dpi(150) end
        if c.cgroup.current_client.manage_ticket < c.manage_ticket then
            status_text = "<"
        else
            status_text = ">"
        end
    else
        if not icon_only then forced_width = dpi(300) end
        for _, pp in ipairs(property_to_text) do
            local key = pp[1]
            if c.saved and c.saved[key] ~= nil then
                prop[key] = c.saved[key]
            elseif c[key] ~= nil then
                prop[key] = c[key]
            end
        end
        for _, pp in ipairs(property_to_text) do
            local key, text = table.unpack(pp)
            if prop[key] == true then
                status_text = status_text .. text
            end
        end
        if status_text == "" and prop["minimized"] then
            status_text = "m"
        end
    end
    widget.forced_width = forced_width
    if icon_only then
        if title_widget and #title_widget.text > 0 then title_widget.text = "" end
        if status_widget and #status_widget.text > 0 then status_widget.text = "" end
        if inline_status_widget then inline_status_widget.text = status_text end
    else
        if title_widget then title_widget.text = c.name or "<Untitled>" end
        if status_widget then status_widget.text = status_text end
        if inline_status_widget and #inline_status_widget.text > 0 then inline_status_widget.text = "" end
    end
    background_widget.context_transformation = {
        focus = client.focus == c or (shared.waffle_selected_client == c and not waffle:autohide()),
        minimized = (c.cgroup and c.cgroup.current_client or c).minimized,
        is_odd = index % 2 == 1
    }
end

local function tasklist_create_function(widget, c, index, objects)
    -- widget.buttons = awful.widget.common.create_buttons(my_tasklist_buttons, {c, widget})
    attach_tasklist_item_buttons(widget, c)
    tasklist_update_function(widget, c, index, objects)
end

local alt_color_cache = {}
local function alt_color(color)
    if alt_color_cache[color] == nil then
        local comp = aux.color.from_string(color)
        for i = 1, 3 do
            if comp[i] > 0.5 then
                comp[i] = comp[i] - 0.1
            else
                comp[i] = comp[i] + 0.1
            end
        end
        alt_color_cache[color] = comp:to_string()
    end
    return alt_color_cache[color]
end

local tasklist_template = {
    {
        {
            {
                {
                    {
                        {
                            {
                                {
                                    widget = awful.widget.clienticon,
                                },
                                {
                                    id = "default_icon",
                                    image = beautiful.client_default_icon,
                                    widget = wibox.widget.imagebox,
                                },
                                widget = fallback,
                            },
                            {
                                id = "inline_status_role",
                                text = "",
                                align = "right",
                                valign = "bottom",
                                outline_size = dpi(2),
                                widget = outlined_textbox,
                            },
                            layout = wibox.layout.stack,
                        },
                        [top_index[shared.vars.bar_position]] = (beautiful.bar_height - beautiful.bar_icon_size) / 2,
                        [bottom_index[shared.vars.bar_position]] = (beautiful.bar_height - beautiful.bar_icon_size) / 2,
                        widget = wibox.container.margin,
                    },
                    {
                        {
                            id = "title_text_role",
                            widget = wibox.widget.textbox,
                        },
                        [left_index[shared.vars.bar_position]] = beautiful.sep_small_size,
                        [right_index[shared.vars.bar_position]] = beautiful.sep_small_size,
                        draw_empty = false,
                        widget = fixed_margin,
                    },
                    {
                        {
                            {
                                id = "status_role",
                                valign = "center",
                                align = "center",
                                widget = wibox.widget.textbox,
                            },
                            direction = direction_index[shared.vars.bar_position] == "horizontal" and "north" or "west",
                            widget = wibox.container.rotate
                        },
                        fg_picker = function (context)
                            if context.selected or context.focus or context.minimized then
                                return beautiful.special_focus
                            else
                                return beautiful.special_normal
                            end
                        end,
                        widget = ocontainer,
                    },
                    layout = wibox.layout.align[direction_index[shared.vars.bar_position]],
                },
                direction = direction_index[shared.vars.bar_position] == "horizontal" and "north" or "west",
                widget = wibox.container.rotate
            },
            [left_index[shared.vars.bar_position]]  = beautiful.sep_small_size,
            [right_index[shared.vars.bar_position]] = beautiful.sep_small_size,
            widget = wibox.container.margin
        },
        layout = wibox.layout.stack,
    },
    id     = "my_background_role",
    fg_picker = function (context)
        if context.selected then
            return beautiful.fg_focus
        elseif context.focus then
            return beautiful.fg_focus
        elseif context.minimized then
            return beautiful.fg_minimize
        else
            return beautiful.fg_normal
        end
    end,
    bg_picker = function (context)
        local ret
        if context.selected then
            return beautiful.bg_focus
        elseif context.focus then
            ret = beautiful.bg_focus
        elseif context.minimized then
            ret = beautiful.bg_minimize
        else
            ret = beautiful.bg_normal
        end
        -- if context.is_odd and not context.focus then
        --     ret = alt_color(ret)
        -- end
        return ret
    end,
    widget = ocontainer,
    create_callback = tasklist_create_function,
    update_callback = tasklist_update_function,
}

function module.create(scr)
    local tasklist -- leave it there for reference inside its definition.
    tasklist = awful.widget.tasklist {
        screen = scr,
        filter = function (c, s)
            if c.cgroup ~= nil then
                c = c.cgroup.current_client
            end
            if not awful.widget.tasklist.filter.currenttags(c, s) then
                return false
            end
            -- WIP - disable the hiding for now
            -- return not (c:isvisible() and shared.vars.hide_clients_with_titlebars and c.has_titlebar)
            return true
        end,
        -- Handled in create_callback
        -- buttons = my_tasklist_buttons,
        style = { font = beautiful.font },
        layout = beautiful.tasklist_layout[direction_index[shared.vars.bar_position]][beautiful.bar_style],
        source = function () return scr:tasklist_clients() end,
        update_function = function (w, b, l, d, clients, args)
            capi.awesome.emit_signal("tasklist::update::before", scr)
            awful.widget.common.list_update(w, b, l, d, clients, args)
        end,
        widget_template = tasklist_template,
    }
    return tasklist
end

return module

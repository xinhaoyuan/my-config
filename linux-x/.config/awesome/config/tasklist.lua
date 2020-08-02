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
local cbg = require("contextual_background")
local border = require("border-theme")
local fixed_margin = require("fixed_margin")
local fixed_place = require("fixed_place")
local fixed_align = require("fixed_align")
local matrix = require("gears.matrix")

local module = {}

local default_icon = gcolor.recolor_image(icons.terminal, beautiful.fg_normal)

local size_index = shared.size_index
local dual_size_index = shared.dual_size_index
local top_index = shared.top_index
local bottom_index = shared.bottom_index
local left_index = shared.left_index
local right_index = shared.right_index
local direction_index = shared.direction_index
local dual_direction_index = shared.dual_direction_index
local gravity_index = shared.gravity_index

-- TODO: deduplicate this with screen.lua {{{
local function with_border(args)
    args = args or nil
    local directions = border.directions{
        args.top and "top",
        args.left and "left",
        args.right and "right",
        args.bottom and "bottom"
    }
    local corners = {
        top_left = args.top and args.left,
        top_right = args.top and args.right,
        bottom_left = args.bottom and args.left,
        bottom_right = args.bottom and args.right
    }
    return wibox.widget {
        {
            {
                args.widget,
                bg = beautiful.bg_normal,
                -- shape = beautiful.border_radius ~= nil and
                --     function (cr, width, height)
                --         rounded_rect_with_corners(cr, width, height,
                --                      beautiful.border_radius -
                --                          beautiful.border_width,
                --                      corners)
                --     end,
                widget = wibox.container.background
            },
            top = args.top and beautiful.border_width,
            left = args.left and beautiful.border_width,
            right = args.right and beautiful.border_width,
            buttom = args.right and beautiful.border_width,
            draw_empty = args.draw_empty,
            widget = fixed_margin,
        },
        bgimage = function (context, cr, width, height)
            border:draw({ -- theme = border_theme,
                    color = beautiful.border_focus }, cr, width, height,
                directions)
        end,
        -- shape = beautiful.border_radius ~= nil and
        --     function (cr, width, height)
        --         rounded_rect_with_corners(cr, width, height,
        --                                   beautiful.border_radius, corners)
        --     end,
        widget = wibox.container.background
    }
end

local function with_top_border(widget)
    return with_border { widget = widget, top = true }
end

local space_filler = wibox.widget {
    forced_width = beautiful.useless_gap,
    widget = wibox.container.constraint
}

local space_filler_with_left_right_borders = wibox.widget {
    {
        forced_width = beautiful.useless_gap + beautiful.border_width * 2,
        widget = wibox.container.constraint,
    },
    bgimage = function (context, cr, width, height)
        -- TODO: Support rotation.
        local total_width = beautiful.border_width
        cr:save()
        cr:rectangle(0, 0, total_width, height)
        cr:clip()
        border:draw({ color = beautiful.border_focus }, cr, total_width, height,
            border.directions{ right_index[shared.var.bar_position], top_index[shared.var.bar_position] })
        cr:restore()
        cr:save()
        cr:translate(width - total_width, 0)
        cr:rectangle(0, 0, total_width, height)
        cr:clip()
        border:draw({ color = beautiful.border_focus }, cr, total_width, height,
            border.directions{ left_index[shared.var.bar_position], top_index[shared.var.bar_position] })
        cr:restore()
    end,
    widget = wibox.container.background
}

local space_filler_with_left_right_borders_no_min = wibox.widget {
    bgimage = function (context, cr, width, height)
        -- TODO: Support rotation.
        local total_width = beautiful.border_width
        cr:save()
        cr:rectangle(0, 0, total_width, height)
        cr:clip()
        border:draw({ color = beautiful.border_focus }, cr, total_width, height,
            border.directions{ right_index[shared.var.bar_position], top_index[shared.var.bar_position] })
        cr:restore()
        cr:save()
        cr:translate(width - total_width, 0)
        cr:rectangle(0, 0, total_width, height)
        cr:clip()
        border:draw({ color = beautiful.border_focus }, cr, total_width, height,
            border.directions{ left_index[shared.var.bar_position], top_index[shared.var.bar_position] })
        cr:restore()
    end,
    widget = wibox.container.background
}

local space_filler_left_with_top_border = with_top_border {
    {
        beautiful.sep_widget,
        forced_width = dpi(10),
        content_fill_vertical = true,
        content_fill_horizontal = true,
        widget = wibox.container.place
    },
    halign = "right",
    widget = fixed_place,
}

local space_filler_right_with_top_border = with_top_border {
    {
        beautiful.sep_widget,
        forced_width = dpi(10),
        content_fill_vertical = true,
        content_fill_horizontal = true,
        widget = wibox.container.place
    },
    halign = "left",
    widget = fixed_place,
}

local space_filler_left = wibox.widget {
    space_filler_with_left_right_borders,
    ["content_fill_horizontal"] = true,
    ["content_fill_vertical"] = true,
    widget = fixed_place
}

local space_filler_right = wibox.widget {
    space_filler_with_left_right_borders,
    ["content_fill_horizontal"] = true,
    ["content_fill_vertical"] = true,
    widget = fixed_place
}

-- }}}

local my_tasklist_buttons = awful.util.table.join(
    awful.button({ }, 1, function (client_and_widget)
            local c = client_and_widget[1]
            if capi.client.focus == client_and_widget[1] then
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
    end),
    awful.button({ }, 2,
        function (client_and_widget)
            local c = client_and_widget[1]
            shared.client.titlebar_enable(c)
        end
    ),
    awful.button({ }, 3,
        function (client_and_widget)
            local c = client_and_widget[1]
            local w = client_and_widget[2]
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
                local m = wgeo.hierarchy:get_matrix_from_device()
                shared.waffle.show_client_waffle(c, { anchor = { x = wb.x + wgeo.x + wgeo.width / 2, y = wb.y + wgeo.y + wgeo.height / 2 } })
            else
                shared.waffle.show_client_waffle(c)
            end
        end
    ),
    awful.button({ }, 4,
        function (client_and_widget)
            local c = client_and_widget[1]
            if not c.maximized then
                shared.client.enlarge(c)
            end
        end
    ),
    awful.button({ }, 5,
        function (client_and_widget)
            local c = client_and_widget[1]
            shared.client.shrink(c)
        end
    )
)

local property_to_text = {
    {"sticky", "S"},
    {"above", "A"},
    {"ontop", "T"},
    {"floating", "F"},
    {"maximized", "M"},
}

local function tasklist_update_function(widget, c, index, objects)
    local sb = widget:get_children_by_id("status_role")[1]
    local bgb = widget:get_children_by_id("my_background_role")[1]
    local title_text_role = widget:get_children_by_id("title_text_role")[1]
    local status_text = ""
    local prop = {}
    if title_text_role ~= nil then
        title_text_role.text = c.name or "<Untitled>"
    end
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
    if sb then
        if #status_text > 0 then
            sb.text = status_text
        else
            sb.text = ""
        end
    end
    bgb:set_context_transform_function({
            focus = client.focus == c,
            selected = shared.waffle_selected_client == c,
            minimized = c.minimized,
            is_odd = index % 2 == 1})
end

local function tasklist_create_function(widget, c, index, objects)
    widget.buttons = awful.widget.common.create_buttons(my_tasklist_buttons, {c, widget})
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
                                widget = awful.widget.clienticon,
                            },
                            {
                                id = "default_icon",
                                image = default_icon,
                                widget = masked_imagebox,
                            },
                            widget = fallback,
                        },
                        [top_index[shared.var.bar_position]] = dpi(2),
                        [bottom_index[shared.var.bar_position]] = dpi(2),
                        [right_index[shared.var.bar_position]] = dpi(4),
                        widget = wibox.container.margin,
                    },
                    {
                        id = "title_text_role",
                        widget = wibox.widget.textbox,
                    },
                    {
                        {
                            {
                                {
                                    id = "status_role",
                                    valign = "center",
                                    align = "center",
                                    widget = wibox.widget.textbox,
                                },
                                direction = direction_index[shared.var.bar_position] == "horizontal" and "north" or "west",
                                widget = wibox.container.rotate
                            },
                            fg_function = function (context)
                                if context.selected or context.focus or context.minimized then
                                    return beautiful.special_focus
                                else
                                    return beautiful.special_normal
                                end
                            end,
                            widget = cbg
                        },
                        left = dpi(4),
                        widget = wibox.container.margin,
                    },
                    layout = wibox.layout.align[direction_index[shared.var.bar_position]],
                },
                direction = direction_index[shared.var.bar_position] == "horizontal" and "north" or "west",
                widget = wibox.container.rotate
            },
            [left_index[shared.var.bar_position]]  = dpi(4),
            [right_index[shared.var.bar_position]] = dpi(4),
            widget = wibox.container.margin
        },
        -- {
        --     {
        --         {
        --             {
        --                 id = "base_action",
        --                 {
        --                     forced_height = beautiful.bar_height,
        --                     forced_width = dpi(30),
        --                     bg_function = function (context)
        --                         local to
        --                         to = beautiful.bg_normal
        --                         if context.is_odd then
        --                             to = alt_color(to)
        --                         end
        --                         local ret = "linear:0,0:" .. tostring(dpi(30)) .. ",0:0," .. to:sub(1, 7) .. "00" .. ":1," .. to:sub(1, 7) .. "ff"
        --                         return ret
        --                     end,
        --                     widget = cbg,
        --                 },
        --                 halign = "right",
        --                 widget = fixed_place,
        --             },
        --             {
        --                 {
        --                     id = "action_container",
        --                     layout = wibox.layout.fixed.horizontal,
        --                 },
        --                 bg_function = function (context)
        --                     local ret
        --                     ret = beautiful.bg_normal
        --                     if context.is_odd then
        --                         ret = alt_color(ret)
        --                     end
        --                     return ret
        --                 end,
        --                 widget = cbg,
        --             },
        --             {
        --                 id = "base_action",
        --                 {
        --                     forced_height = beautiful.bar_height,
        --                     forced_width = dpi(30),
        --                     bg_function = function (context)
        --                         local to
        --                         to = beautiful.bg_normal
        --                         if context.is_odd then
        --                             to = alt_color(to)
        --                         end
        --                         local ret = "linear:0,0:" .. tostring(dpi(30)) .. ",0:0," .. to:sub(1, 7) .. "ff" .. ":1," .. to:sub(1, 7) .. "00"
        --                         return ret
        --                     end,
        --                     widget = cbg,
        --                 },
        --                 halign = "left",
        --                 widget = fixed_place,
        --             },
        --             expand = "outside",
        --             widget = fixed_align.horizontal,
        --         },
        --         id = "action_layer",
        --         visible = false,
        --         -- bg_function = function (context)
        --         --     local ret
        --         --     if context.focus then
        --         --         ret = beautiful.bg_focus
        --         --     elseif context.minimized then
        --         --         ret = beautiful.bg_minimize
        --         --     else
        --         --         ret = beautiful.bg_normal
        --         --     end
        --         --     if context.is_odd then
        --         --         ret = alt_color(ret)
        --         --     end
        --         --     return ret
        --         -- end,
        --         widget = cbg,
        --     },
        --     content_fill_horizontal = true,
        --     --- fill_horizontal = true,
        --     widget = fixed_place,
        -- },
        layout = wibox.layout.stack,
    },
    id     = "my_background_role",
    fg_function = function (context)
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
    bg_function = function (context)
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
    widget = cbg,
    create_callback = tasklist_create_function,
    update_callback = tasklist_update_function,
}

function module.create(scr)
    local tasklist -- leave it there for reference inside its definition.
    tasklist = awful.widget.tasklist {
        screen = scr,
        filter = function (c, s)
            if not awful.widget.tasklist.filter.currenttags(c, s) then
                return false
            end
            -- WIP - disable the hiding for now
            -- return not (c:isvisible() and shared.var.hide_clients_with_titlebars and c.has_titlebar)
            return true
        end,
        -- Handled in create_callback
        -- buttons = my_tasklist_buttons,
        style = { font = beautiful.font },
        layout = beautiful.tasklist_layout[direction_index[shared.var.bar_position]][beautiful.bar_style],
        source = function ()
            -- Sort clients with their constant ids to make the order stable.
            local cls = awful.widget.tasklist.source.all_clients()
            table.sort(cls,
                       function (a, b)
                           -- this makes minimized windows appear at last
                           -- if a.minimized ~= b.minimized then return b.minimized else return a.window < b.window end
                           return a.manage_ticket < b.manage_ticket
                       end
            )
            return cls
        end,
        update_function = function (w, b, l, d, clients, args)
            if beautiful.bar_style == "auto" then
                local should_expand = false
                for _, c in ipairs(clients) do
                    if c:isvisible() and c.maximized then
                        should_expand = true
                        break
                    end
                end

                -- tasklist.expand_space = should_expand
                local space_filler_left = scr.widgets.wibar.widget:get_children_by_id("space_filler_left")[1]
                local space_filler_right = scr.widgets.wibar.widget:get_children_by_id("space_filler_right")[1]
                if should_expand then
                    space_filler_left:set_children({space_filler_left_with_top_border})
                    space_filler_right:set_children({space_filler_right_with_top_border})
                elseif #clients == 0 then
                    space_filler_left:set_children({space_filler_with_left_right_borders_no_min})
                    space_filler_right:set_children({space_filler_with_left_right_borders_no_min})
                else
                    space_filler_left:set_children({space_filler_with_left_right_borders})
                    space_filler_right:set_children({space_filler_with_left_right_borders})
                end
            elseif beautiful.bar_style == "split" then
                local space_filler_left = scr.widgets.wibar.widget:get_children_by_id("space_filler_left")[1]
                local space_filler_right = scr.widgets.wibar.widget:get_children_by_id("space_filler_right")[1]
                if #clients == 0 then
                    space_filler_left:set_children({space_filler_with_left_right_borders_no_min})
                    space_filler_right:set_children({space_filler_with_left_right_borders_no_min})
                else
                    space_filler_left:set_children({space_filler_with_left_right_borders})
                    space_filler_right:set_children({space_filler_with_left_right_borders})
                end
            end

            awful.widget.common.list_update(w, b, l, d, clients, args)
        end,
        widget_template = tasklist_template,
    }
    return tasklist
end

return module

local capi = {
    screen = screen,
    client = client,
}
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local watcher = function(args)
    assert(args.layout, "args.layout must not be nil")
    local layout = args.layout
    local cleanup_cb = args.cleanup_cb
    local before_arrange_cb = args.before_arrange_cb
    local after_arrange_cb = args.after_arrange_cb

    local clean_up
    local tag_data = setmetatable({}, {__mode = "k"})

    clean_up = function (tag)
        if tag_data[tag].regsitered then
            tag_data[tag].regsitered = false
            tag:disconnect_signal("property::layout", clean_up)
            tag:connect_signal("property::selected", clean_up)
            if cleanup_cb then cleanup_cb(tag) end
        end
    end

    local function arrange(p)
        p.tag = p.tag or capi.screen[p.screen].selected_tag 
        local tag = p.tag
        if not tag_data[tag] then tag_data[tag] = {} end
        if not tag_data[tag].registered then
            tag_data[tag].regsitered = true
            tag:connect_signal("property::layout", clean_up)
            tag:connect_signal("property::selected", clean_up)
        end

        if before_arrange_cb then before_arrange_cb(p) end
        layout.arrange(p)
        if after_arrange_cb then after_arrage_cb(p) end
    end

    return {
        name = args.name or layout.name,
        arrange = arrange,
        resize_handler = args.layout.resize_handler,
        mouse_resize_handler = args.mouse_resize_handler,
    }
end

local smart_boxes = setmetatable({}, {__mode = "k"})
local function smart_check_client_focus_geometry(c, boxes)
    if c == nil or not c.valid or capi.client.focus ~= c or not c:isvisible() then return end
    for box, _ in pairs(boxes or smart_boxes) do
        if box.valid then 
            if c.x + c.width + c.border_width * 2 > box.x and box.x + box.width > c.x and
                c.y + c.height + c.border_width * 2 > box.y and box.y + box.height > c.y then
                box.ontop = false
            else
                box.ontop = true
            end
        end
    end
end
capi.client.connect_signal("property::maximized", smart_check_client_focus_geometry)
capi.client.connect_signal("geometry", smart_check_client_focus_geometry)
capi.client.connect_signal("focus", smart_check_client_focus_geometry)

local function smart_wibox(args)
    local box = wibox(args)
    smart_boxes[box] = true
    smart_check_client_focus_geometry(capi.client.focus, {[box] = true})
    return box
end

local function default_set_bar_clients(bar, clients)
    if #clients == 0 then
        bar.visible = false
    else
        bar.visible = true
    end

    if not bar.widget then
        bar.widget = wibox.layout.flex.horizontal()
    end
    bar.widget:reset()

    for _, c in ipairs(clients) do
        bar.widget:add(
            wibox.widget{
                text = c.name,
                align = "center",
                widget = wibox.widget.textbox,
            }
        )
    end
end

local tabber = function(args)
    assert(args.layout, "args.layout must not be nil") 
    assert(args.bar_size, "args.bar_size must not be nil") 
    local layout = args.layout
    local set_bar_clients = args.set_bar_clients or default_set_bar_clients
    
    tag_data = setmetatable({}, {__mode = "k"})
    return watcher{
        layout = layout,
        cleanup_cb = function (tag)
            local data = tag_data[tag]
            -- TODO Maybe delay the real cleanup.
            data.bar_wibox.visible = false
            tag_data[tag] = nil
        end,
        before_arrange_cb = function (p)
            local tag = p.tag or capi.screen[p.screen].selected_tag
            local data = tag_data[tag]
            if not data then
                data = {}
                tag_data[tag] = data
            end

            local height = args.bar_size
            -- TODO Hack related to layout-machi.
            local gap_included = p.workarea.x < p.geometry.x
            local space = height + p.useless_gap * 2

            if data.bar_wibox == nil then
                data.bar_wibox = smart_wibox{
                    x = gap_included and p.geometry.x or p.geometry.x + p.useless_gap * 2,
                    y = gap_included and p.geometry.y or p.geometry.y + p.useless_gap * 2,
                    width = gap_included and p.geometry.width or p.geometry.width - p.useless_gap * 4,
                    height = height,
                    screen = p.screen,
                    bg = args.bar_bg,
                    ontop = true,
                    visible = true,
                }
            end

            if #p.clients >= (args.bar_min_clients or 0) then
                p.workarea.y = p.workarea.y + space
                p.workarea.height = p.workarea.height - space

                set_bar_clients(data.bar_wibox, p.clients)
            else
                data.bar_wibox.visible = false
            end
        end,
    }
end

return tabber

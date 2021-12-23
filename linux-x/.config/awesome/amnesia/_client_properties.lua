-- Keep per tag/layout properties of clients.
-- See the local `properties` for the saved properties. 

local capi = {
    client = client,
    tag = tag,
    screen = screen,
}
local gtimer = require("gears.timer")
local data_per_tag_layout = setmetatable({}, {__mode = "k"})

local function get_data(tag, layout)
    local data_per_layout = data_per_tag_layout[tag]
    if data_per_layout == nil then
        data_per_layout = setmetatable({}, {__mode = "k"})
        data_per_tag_layout[tag] = data_per_layout
    end
    local data = data_per_layout[layout]
    if data == nil then
        data = {
            client_info = setmetatable({}, {__mode = "k"}),
        }
        data_per_layout[layout] = data
    end
    return data
end

local properties = {
    "maximized",
    "maximized_horizontal",
    "maximized_vertical",
    "minimized",
    "floating",
}

local during_screen_refresh = false
local function create_property_callback(property)
    return function (client)
        if during_screen_refresh then return end
        local tag = client.screen.selected_tag
        local layout = tag.layout
        local data = get_data(tag, layout)
        local info = data.client_info[client]
        if info == nil then return end
        info[property] = client[property]
    end
end
for i, property in ipairs(properties) do
    capi.client.connect_signal(
        "property::"..property, create_property_callback(property))
end

capi.client.connect_signal(
    "tagged", function (client, tag)
        local layout = tag.layout
        local data = get_data(tag, layout)
        local info = data.client_info[client]
        if info == nil then
            info = {}
            data.client_info[client] = info
        end
        for _, property in ipairs(properties) do
            info[property] = client[property]
        end
    end)

local screen_last_refreshed_data = setmetatable({}, {__mode = "k"})
local screen_refresh_scheduled = setmetatable({}, {__mode = "k"})
local function screen_refresh(s)
    screen_refresh_scheduled[s] = nil
    during_screen_refresh = true
    local tag = s.selected_tag
    local layout = tag.layout
    local data = get_data(tag, layout)
    screen_last_refreshed_data[s] = data
    local invalid_clients = {}
    local clients = s.all_clients
    for _, client in ipairs(clients) do
        info = data.client_info[client]
        if info == nil then
            info = {}
            for _, property in ipairs(properties) do
                info[property] = client[property]
            end
            data.client_info[client] = info
        else
            for _, property in ipairs(properties) do
                client[property] = info[property]
            end
        end
    end
    during_screen_refresh = false
end

local function maybe_call_screen_refresh(tag)
    if not tag.screen then
        -- May happen with virtual tags.
        -- print(require("debug").traceback());
        return
    end
    local data = get_data(tag, tag.layout)
    if data == screen_last_refreshed_data[tag.screen] then return end
    if screen_refresh_scheduled[tag.screen] then return end
    screen_refresh_scheduled[tag.screen] = true
    gtimer.delayed_call(screen_refresh, tag.screen)
end
capi.tag.connect_signal("property::selected", maybe_call_screen_refresh)
capi.tag.connect_signal("property::layout", maybe_call_screen_refresh)


return nil

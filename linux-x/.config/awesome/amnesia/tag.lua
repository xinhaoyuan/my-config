-- Keep per tag properties of clients.
-- See the local `properties` for the saved properties. 

local capi = {
    client = client,
    tag = tag,
    screen = screen,
}
local gtimer = require("gears.timer")
local data_per_tag = setmetatable({}, {__mode = "k"})

local function get_data(tag, can_create)
    local data = data_per_tag[tag]
    if data == nil and can_create then
        data = {
            client_info = setmetatable({}, {__mode = "k"}),
        }
        data_per_tag[tag] = data
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
local function tagged_callback(client, tag)
    local data = get_data(tag, true)
    local info = {}
    data.client_info[client] = info
    for _, property in ipairs(properties) do
        info[#info + 1] = client[property]
    end
end
capi.client.connect_signal("tagged", tagged_callback)

local during_screen_restore = false
local function create_property_callback(i, property)
    return function (client)
        if during_screen_restore then return end
        local data = get_data(client.screen.selected_tag, false)
        if not data then return end
        local info = data.client_info[client]
        if info == nil then return end
        info[i] = client[property]
    end
end
for i, property in ipairs(properties) do
    capi.client.connect_signal(
        "property::"..property, create_property_callback(i, property))
end

local screen_last_selected_tag = setmetatable({}, {__mode = "k"})
local screen_restore_callback_scheduled = setmetatable({}, {__mode = "k"})
local function screen_restore_callback(s)
    screen_restore_callback_scheduled[s] = nil
    screen_last_selected_tag[s] = s.selected_tag
    during_screen_restore = true
    local tag = s.selected_tag
    local data = get_data(tag, false)
    if not data then return end
    local invalid_clients = {}
    for c, info in pairs(data.client_info) do
        if c.valid then
            for i, property in ipairs(properties) do
                c[property] = info[i]
            end
        else
            invalid_clients[#invalid_clients + 1] = c
        end
    end
    for _, c in ipairs(invalid_clients) do
        data.client_info[c] = nil
    end
    during_screen_restore = false
end

local function tag_select_callback(tag)
    if tag.screen.selected_tag == screen_last_selected_tag[s] then return end
    if screen_restore_callback_scheduled[tag.screen] then return end
    screen_restore_callback_scheduled[tag.screen] = true
    gtimer.delayed_call(screen_restore_callback, tag.screen)
end
capi.tag.connect_signal("property::selected", tag_select_callback)

return nil

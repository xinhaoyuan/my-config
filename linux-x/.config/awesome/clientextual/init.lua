local capi = {
    client = client,
    tag = tag,
    screen = screen,
}
local gtimer = require("gears.timer")
local module = {
    replace_awful_layouts = true,
    ignore_layout = true,
    ignore_tag = false,
}
local logging_error = 0
local logging_warning = 1
local logging_info = 2
local logging_noise = 3
module.log_level = logging_warning

-- Instance in this module means a wrapped layout.
local data_per_instance_tag = setmetatable({}, {__mode = "k"})
local instance_map = setmetatable({}, {__mode = "k"})
local __default_tag = {}
local __default_instance = {}

local function get_data_per_tag(instance)
    if module.ignore_layout then
        instance = __default_instance
    end
    local data_per_tag = data_per_instance_tag[instance]
    if data_per_tag == nil then
        data_per_tag = setmetatable({}, {__mode = "k"})
        data_per_instance_tag[instance] = data_per_tag
    end
    return data_per_tag
end

local function get_data(instance, tag)
    local data_per_tag = get_data_per_tag(instance)
    if module.ignore_tag then
        tag = __default_tag
    end
    local data = data_per_tag[tag]
    if data == nil then
        data = {
            client_order = setmetatable({}, {__mode = "k"}),
            client_info = setmetatable({}, {__mode = "k"}),
            max_rank = 0,
        }
        data_per_tag[tag] = data
    end
    return data
end

local function sort_clients(clients, data)
    local client_order = data.client_order
    local rank_slot = {}
    for i, c in pairs(clients) do
        local rank = client_order[c].rank
        assert(rank_slot[rank] == nil)
        rank_slot[rank] = c
    end
    local index = 0
    for i = 1, data.max_rank do
        if rank_slot[i] then
            index = index + 1
            clients[index] = rank_slot[i]
        end
    end
    -- Alternatively, use simple sort.
    --[[
    table.sort(
        clients, function (a, b)
            return client_order[a].rank > client_order[b].rank
        end)
    --]]
end

local function try_gc(data)
    local client_order = data.client_order
    if data.max_rank <= capi.client.instances() * 2 then
        return
    end
    local all_clients = {}
    for c, _ in pairs(client_order) do
        if c.valid then
            all_clients[#all_clients + 1] = c
        end
    end
    sort_clients(all_clients, data)
    for i, c in ipairs(all_clients) do
        client_order[c].rank = i
    end
    data.max_rank = #all_clients
end

local function swapped_callback(source, target, is_source)
    if source.immobilized_horizontal or source.immobilized_vertical or
        target.immobilized_horizontal or target.immobilized_vertical then
        return
    end
    local source_data = get_data(source.screen.selected_tag.layout, source.screen.selected_tag)
    local target_data = get_data(target.screen.selected_tag.layout, target.screen.selected_tag)
    local source_order = source_data and source_data.client_order[source]
    local target_order = target_data and target_data.client_order[target]
    -- Avoid swapping twice.
    if source_order and target_order and is_source then return end
    if source_order then
        source_data.client_order[target] = source_order
    end
    if target_order then
        target_data.client_order[source] = target_order
    end
end
capi.client.connect_signal("swapped", swapped_callback)

local function wrap_layout(base_layout)
    if instance_map[base_layout] then return instance_map[base_layout] end
    local instance = setmetatable({}, {__index = base_layout})
    local data_per_tag = get_data_per_tag(base_layout)
    data_per_instance_tag[instance] = data_per_tag
    instance_map[base_layout] = instance
    instance_map[instance] = instance

    function instance.arrange(params)
        local tag = params.tag or
            (params.screen and capi.screen[params.screen].selected_tag) or
            __default_tag
        local data = get_data(instance, tag)
        local client_order = data.client_order
        local clients = params.clients
        for _, c in ipairs(clients) do
            local order = client_order[c]
            if order == nil then
                data.max_rank = data.max_rank + 1
                order = { rank = data.max_rank }
                client_order[c] = order
            end
        end
        sort_clients(clients, data)
        try_gc(data)
        base_layout.arrange(params)
    end
    return instance
end
module.wrap_layout = wrap_layout

local properties = {
    "maximized_horizontal",
    "maximized_vertical",
    "maximized",
    "minimized",
    "floating",
    -- Geometry has to be the last.
    "x",
    "y",
    "width",
    "height",
}
local signal_watched_properties = {
    ["property::geometry"] = {"x", "y", "width", "height"}
}

local during_screen_refresh = false
local client_properties_to_save = setmetatable({}, {__mode = "k"})
local client_tagged_callback_tags = setmetatable({}, {__mode = "k"})
local function save_client_properties(client)
    local properties_to_save = client_properties_to_save[client]
    client_properties_to_save[client] = nil
    if not client.valid then return end
    -- Do not save properties when switching tags. Saving will be done there if needed.
    if client_tagged_callback_tags[client] ~= nil then
        if module.log_level >= logging_info then
            print("Skipping saving client properties because tagged callback is not empty.")
        end
        return
    end
    local tag = client.screen.selected_tag
    local layout = tag.layout
    local data = get_data(layout, tag)
    if data == nil then return end
    local info = data.client_info[client]
    if info == nil then return end
    for property, _ in pairs(properties_to_save) do
        if module.log_level >= logging_info then
            print("Saved client property", client, property, client[property], tag.name, tag.screen.index, layout, info)
        end
        info[property] = client[property]
    end
end
for i, property in ipairs(properties) do
    capi.client.connect_signal(
        "property::"..property, function (client)
            if during_screen_refresh then return end
            if client_properties_to_save[client] == nil then
                client_properties_to_save[client] = {}
                gtimer.delayed_call(save_client_properties, client)
            end
            -- print("signal client property", client, property)
            client_properties_to_save[client][property] = true
        end)
end
for signal, properties in pairs(signal_watched_properties) do
    capi.client.connect_signal(
        signal, function (client)
            if during_screen_refresh then return end
            if client_properties_to_save[client] == nil then
                client_properties_to_save[client] = {}
                gtimer.delayed_call(save_client_properties, client)
            end
            for _, property in ipairs(properties) do
                -- print("signal client property", client, property)
                client_properties_to_save[client][property] = true
            end
        end)
end

local function sync_client_info(client_info, client)
    local info = client_info[client]
    if info == nil then
        info = {}
        for _, property in ipairs(properties) do
            if module.log_level >= logging_info then
                print("Storing client property", client, property, client[property], info)
            end
            info[property] = client[property]
        end
        client_info[client] = info
    else
        for _, property in ipairs(properties) do
            -- Avoid unnecessary hooks.
            if ((property == "x" or property == "width") and
                (client.maximized or client.maximized_horizontal)) or
                ((property == "y" or property == "height") and
                 (client.maximized or client.maximized_vertical)) then
                if module.log_level >= logging_noise then
                    print("Skipping geometry property for maximized client", client, property, info[property], info)
                end
            elseif client[property] ~= info[property] then
                if module.log_level >= logging_info then
                    print("Restoring client property", client, property, info[property], info)
                end
                client[property] = info[property]
            else
                if module.log_level >= logging_noise then
                    print("Skipping unchanged client property", client, property, info[property], info)
                end
            end
        end
    end
end

local function client_tagged_callback(client)
    local callback_tags = client_tagged_callback_tags[client]
    client_tagged_callback_tags[client] = nil
    if not client.valid then return end
    local tag = client.screen.selected_tag
    if not callback_tags[tag] then
        if module.log_level >= logging_info then
            print("Skipping client tag because it is not the main tag of the screen")
        end
        return
    end
    local layout = tag.layout
    local data = get_data(layout, tag)
    if data == nil then return end
    if module.log_level >= logging_info then
        print("Syncing client info", client, tag.name, tag.screen.index, layout)
    end
    sync_client_info(data.client_info, client)
end
capi.client.connect_signal(
    "tagged", function (client, tag)
        if client_tagged_callback_tags[client] == nil then
            client_tagged_callback_tags[client] = {}
            -- print("trigger client tagged callback", client)
            gtimer.delayed_call(client_tagged_callback, client)
        end
        client_tagged_callback_tags[client][tag] = true
    end)
-- This is needed since `tagged` may be called after some client properties were synced, which is too late.
capi.client.connect_signal(
    "property::screen", function (client)
        local tag = client.screen.selected_tag
        if client_tagged_callback_tags[client] == nil then
            client_tagged_callback_tags[client] = {}
            -- print("trigger client tagged callback", client)
            gtimer.delayed_call(client_tagged_callback, client)
        end
        client_tagged_callback_tags[client][tag] = true
    end)

local screen_last_refreshed_data = setmetatable({}, {__mode = "k"})
local screen_refresh_scheduled = setmetatable({}, {__mode = "k"})
local function screen_refresh(s)
    screen_refresh_scheduled[s] = nil
    during_screen_refresh = true
    local tag = s.selected_tag
    local layout = tag.layout
    local data = get_data(layout, tag)
    if data == nil then return end
    screen_last_refreshed_data[s] = data
    local clients = s.all_clients
    for _, client in ipairs(clients) do
        if module.log_level >= logging_info then
            print("Syncing client info", client, tag.name, tag.screen.index, layout)
        end
        sync_client_info(data.client_info, client)
    end
    during_screen_refresh = false
end

local function is_virtual(tag) return tag.screen == nil end

local function maybe_call_screen_refresh(tag)
    if is_virtual(tag) then return end
    local data = get_data(tag.layout, tag)
    if data == nil then return end
    if data == screen_last_refreshed_data[tag.screen] then return end
    if screen_refresh_scheduled[tag.screen] then return end
    screen_refresh_scheduled[tag.screen] = true
    gtimer.delayed_call(screen_refresh, tag.screen)
end
capi.tag.connect_signal("property::selected", maybe_call_screen_refresh)
capi.tag.connect_signal("property::layout", maybe_call_screen_refresh)

function module.init(settings)
    if type(settings) == "table" then
        for k, v in pairs(settings) do
            if module[k] then
                module[k] = v
            end
        end
    end

    if module.replace_awful_layouts then
        local alayout = require("awful.layout")
        local original_alayout_get = alayout.get
        alayout.get = function(args)
            local original_layout = original_alayout_get(args)
            if module.log_level >= logging_info then
                print("get wrapped layout for", original_layout)
            end
            return wrap_layout(original_layout)
        end
    end
end

setmetatable(module, {__call = function (self, args, ...) self.init(args, ...) end})
return module

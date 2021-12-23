-- Wraps a layout to keep an internal order of the clients.

local capi = {
    screen = screen,
    client = client,
}
local module = {}

local __default_tag_object = {}
local data_per_instance_tag = setmetatable({}, {__mode = "k"})

local function get_data(instance, tag, can_create)
    local data_per_tag = data_per_instance_tag[instance]
    if data_per_tag == nil then return nil end
    local data = data_per_tag[tag]
    if data == nil and can_create then
        data = {
            client_info = setmetatable({}, {__mode = "k"}),
            ticket_count = 0,
        }
        data_per_tag[tag] = data
    end
    return data
end

local function swapped_callback(source, target, is_source)
    if source.immobilized_horizontal or source.immobilized_vertical or
        target.immobilized_horizontal or target.immobilized_vertical then
        return
    end
    local source_data = get_data(source.screen.selected_tag.layout, source.screen.selected_tag)
    local target_data = get_data(target.screen.selected_tag.layout, target.screen.selected_tag)
    local source_info = source_data and source_data.client_info[source]
    local target_info = target_data and target_data.client_info[target]
    -- Avoid swapping twice.
    if source_info and target_info and is_source then return end
    if source_info then
        source_data.client_info[target] = source_info
    end
    if target_info then
        target_data.client_info[source] = target_info
    end
end
capi.client.connect_signal("swapped", swapped_callback)

function module.wrap(args)
    assert(type(args) == "table", "args must be a table")
    assert(args.layout, "args.layout must be non-nil")
    local base_layout = args.layout

    local instance = setmetatable({name = args.name}, {__index = base_layout})
    data_per_instance_tag[instance] = setmetatable({}, {__mode = "k"})

    local function sort_clients(clients, client_info)
        table.sort(
            clients, function (a, b)
                if args.new_client_at_front then
                    return client_info[a].order > client_info[b].order
                else
                    return client_info[a].order < client_info[b].order
                end
            end)
    end

    local function try_gc(data)
        local client_info = data.client_info
        if data.ticket_count > capi.client.instances() * 2 then
            local all_clients = {}
            local invalid_clients = {}
            for c, _ in pairs(client_info) do
                if c.valid then
                    all_clients[#all_clients + 1] = c
                else
                    invalid_clients[#invalid_clients + 1] = c
                end
            end
            sort_clients(all_clients, client_info)
            for i, c in ipairs(all_clients) do
                client_info[c].order = i
            end
            data.ticket_count = #all_clients
            for i, c in ipairs(invalid_clients) do
                client_info[c] = nil
            end
        end
    end

    function instance.arrange(params)
        local tag = params.tag or
            (params.screen and capi.screen[params.screen].selected_tag) or
            __default_tag_object
        local data = get_data(instance, tag, true)
        local client_info = data.client_info
        local clients = params.clients
        for _, c in ipairs(clients) do
            local info = client_info[c]
            if info == nil then
                data.ticket_count = data.ticket_count + 1
                info = { order = data.ticket_count }
                client_info[c] = info
            end
        end
        sort_clients(clients, client_info)
        try_gc(data)
        base_layout.arrange(params)
    end
    return instance
end

return module

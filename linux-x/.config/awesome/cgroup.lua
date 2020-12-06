-- A client grouping module inspired by bling.module.tabbed.

local gears = require('gears')
local awful = require('awful')

-- A cgroup object contains the following fields:
--
--   focused_client
--   clients
--
-- with the following methods (and signals with the same names/parameters):
--
--   attach(client)
--   detach(client)
--   switch(client), the signal is switch(from, to)
--

local cgroup = {}

local function pick_other_client(cgroup)
    local pick = nil
    for c, _ in pairs(cgroup.clients) do
        if c ~= cgroup.current_client then
            if pick == nil or pick.focus_timestamp < c.focus_timestamp then
                pick = c
            end
        end
    end
    return pick
end

local function sync_clients(from_client, to_client)
    local properties_to_sync = {
        'floating', 'above', 'below', 'maximized_vertical', 'maximized_horizontal', 'maximized', 'fullscreen', 'minimized',
    }
    for _, prop in ipairs(properties_to_sync) do
        local tomb_prop = 'tomb_'..prop
        if from_client[tomb_prop] ~= nil then
            to_client[prop] = from_client[tomb_prop]
        else
            to_client[prop] = from_client[prop]
        end
    end
    to_client:geometry(from_client:geometry())
    to_client:tags(from_client.tomb_tags or from_client:tags())
    if from_client.tomb_focused then
        client.focus = to_client
    end
end

local function cgroup_on_client_unmanage(client)
    if client.cgroup == nil then return end
    client.cgroup:detach(client)
end

local function cgroup_on_client_minimized(client)
    if client.cgroup == nil then return end
    if not client.minimized then
        client.cgroup:switch(client, true)
    end
end

function cgroup:detach(client)
    if not self.clients[client] then return end

    self.clients[client] = nil
    client.cgroup = nil
    if client == self.current_client then
        local replacement = pick_other_client(self)
        if replacement ~= nil then
            self:switch(replacement)
        end
    else
        sync_clients(client, self.current_client)
    end

    client:disconnect_signal('unmanage', cgroup_on_client_unmanage)
    client:disconnect_signal('property::minimized', cgroup_on_client_minimized)

    self:emit_signal('detach', client)
end

function cgroup:attach(client)
    if self.clients[client] then return end

    if client.cgroup ~= nil then
        client.cgroup:detach(client)
    end
    client.cgroup = self

    self.clients[client] = true
    if self.current_client == nil then
        self.current_client = client
    else
        client.minimized = true
    end

    client:connect_signal('unmanage', cgroup_on_client_unmanage)
    client:connect_signal('property::minimized', cgroup_on_client_minimized)

    self:emit_signal('attach', client)
end

function cgroup:switch(client, force_show)
    if self.in_switch or not self.clients[client] or self.current_client == client then
        return
    end
    self.in_switch = true

    local old_client = self.current_client
    self.current_client = client

    sync_clients(old_client, client)
    if old_client.tomb then
        client.focus_timestamp = old_client.focus_timestamp
    end
    if self.clients[old_client] then
        old_client.minimized = true
    end
    if force_show then
        client.minimized = false
    end

    self.in_switch = false

    self:emit_signal('switch', old_client, client)
end

local function cgroup_new()
    local result = gears.object { class = cgroup }
    result.clients = {}
    return result
end

local function cgroup_pull(host, guest)
    if host.cgroup == nil then
        cgroup_new():attach(host)
    end

    host.cgroup:attach(guest)
end

return {
    new = cgroup_new,
    pull = cgroup_pull,
}

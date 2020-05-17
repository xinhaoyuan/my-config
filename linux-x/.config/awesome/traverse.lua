local gtimer = require("gears.timer")

local function path_append(component, path)
    return {component, path}
end

local function path_tostring(path)
    local path_string = ""
    while path ~= nil do
        path_string = path[1] .. path_string
        path = path[2]
    end
    return path_string
end

local traverse = {}

local orig_tostring = tostring
local function tostring(o)
    local success, value = pcall(function()
        return orig_tostring(o)
    end)
    return success and value or "tostring-error"
end

function traverse.new(output_filename, age_threshold)
    local self = setmetatable({
            discover_timestamp = setmetatable({}, { __mode = "k"}),
            age_threshold = age_threshold,
            output = io.open(output_filename, "w"),
    }, { __index = traverse })
    return self
end

function traverse:before_scan(options)
    collectgarbage("collect")
    self.counts = {
        boolean = 0,
        number = 0,
        string = 0,
        table = 0,
        ["function"] = 0,
        key = 0,
        button = 0,
        userdata = 0,
        drawable = 0,
        tag = 0,
        screen = 0,
        drawin = 0,
        client = 0,
        old = 0,
    }
    self.options = options or {}
    self.string_length = 0
    self.table_entries = 0
    self.scan_timestamp = os.time()
    self.reached = setmetatable({}, { __mode = "k"})
    self.reached[traverse] = true
    self.reached[self] = true
    self.reached[self.options] = true
    self.reached[self.reached] = true
    self.reached[self.discover_timestamp] = true
    self.reached[self.output] = true
end

function traverse:on_reached_object(object, path)
    local timestamp = self.discover_timestamp[object]
    local is_old = false
    if self.options.mark_ignore then
        self.discover_timestamp[object] = -1
        timestamp = -1
    elseif timestamp == nil then
        self.discover_timestamp[object] = self.scan_timestamp
        timestamp = self.scan_timestamp
    else
        is_old = timestamp >= 0 and self.scan_timestamp - timestamp >= self.age_threshold
        if is_old then
            self.counts.old = self.counts.old + 1
        end
    end
    if timestamp >= 0 and (self.options.dump or (is_old and self.options.dump_old)) then
        self:print("Age "..(self.scan_timestamp - timestamp).."s, "..tostring(object)..": "..path_tostring(path))
    end
end

function traverse:print(...)
    local args = {...}
    for i = 1, #args do
        if i > 1 then self.output:write(" ") end
        self.output:write(tostring(args[i]))
    end
    self.output:write("\n")
end

function traverse:flush()
    self.output:flush()
end

function traverse:print_stat()
    local p = {}
    for k, v in pairs(self.counts) do
        table.insert(p, string.format("%-10s: ", k) .. v)
    end
    table.sort(p)

    self:print("Seen objects:")
    for _, v in ipairs(p) do
        self:print(v)
    end
    self:print("Total string length: " .. self.string_length)
    self:print("Total table entries: " .. self.table_entries)
    self:print()
end

function traverse:scan(options)
    self:before_scan(options)
    self:traverse(_G, path_append("_G"))
    self:traverse(debug.getregistry(), path_append("registry"))
    self:print_stat()
    self:flush()
end

function traverse:traverse(obj, name)
    local todo = { { obj, name or "" } }
    while #todo ~= 0 do
        local offset = 1
        while offset <= #todo do
            -- table.remove(todo, 1) is too slow...
            local elem = todo[offset]
            todo[offset] = nil
            offset = offset + 1

            self:_traverse(elem[1], elem[2], todo)
        end
        local next_todo = {}
        for _, v in pairs(todo) do
            if v ~= nil then
                table.insert(next_todo, v)
            end
        end
        todo = next_todo
    end
end

function traverse:need_traverse(obj, path, todo)
    if self.reached[obj] then return end
    self.reached[obj] = true
    self:on_reached_object(obj, path)
    table.insert(todo, { obj, path })
end

function traverse:_traverse(obj, path, todo)
    path = path or ""

    local t = type(obj)
    if not self["traverse_" .. t] then
        local c = self.counts[t] or 0
        self.counts[t] = c + 1
        t = "unknown"
    else
        self.counts[t] = self.counts[t] + 1
    end

    self["traverse_" .. t](self, obj, path, todo)

    local mt = getmetatable(obj)
    if mt then
        self:need_traverse(mt, path_append("(mt)", path), todo)
    end

    if debug.getuservalue ~= nil then
        local uv = debug.getuservalue(obj)
        if uv then
            self:need_traverse(uv, path_append("(uv)", path), todo)
        end
    end

    if debug.getfenv ~= nil then
        local fenv = debug.getfenv(obj)
        if fenv then
            self:need_traverse(fenv, path_append("(fenv)", path), todo)
        end
    end
end

function traverse:traverse_unknown(obj, path, todo)
    -- self:print("unknown", type(obj), tostring(obj), path)
    pcall(self.traverse_table, self, obj, path, todo)
end

local function nop() end
traverse.traverse_boolean = nop
traverse.traverse_number = nop
-- traverse.traverse_key = nop
-- traverse.traverse_userdata = nop
-- traverse.traverse_button = nop
-- traverse.traverse_drawable = nop
-- traverse.traverse_tag = nop
-- traverse.traverse_screen = nop
-- traverse.traverse_drawin = nop
-- traverse.traverse_client = nop
-- traverse.traverse_screen = nop

traverse.traverse_screen = traverse.traverse_unknown

function traverse:traverse_string(obj)
    self.string_length = self.string_length + #obj + 1
end

function traverse:traverse_table(obj, path, todo)
    if rawget(obj, "print_me") then self:print(obj, path_tostring(path)) end
    local mt = getmetatable(obj) or {}
    if type(mt) ~= "table" then mt = {} end
    local mode = rawget(mt, "__mode") or ""
    local keys = not mode:find("k")
    local values = not mode:find("v")
    --keys, values = true, true
    local k = nil
    while true do
        local v
        k, v = next(obj, k)
        if k == nil then
            break
        end
        if keys then
            if tostring(k) == nil then self:print(path_tostring(path), debug.traceback()) self:print(k) end
            self:need_traverse(k, path_append(".k(" .. tostring(k) .. ")", path), todo)
        end
        if values then
            self:need_traverse(v, path_append("[" .. tostring(k) .. "]", path), todo)
        end
        self.table_entries = self.table_entries + 1
    end
end

function traverse:traverse_function(obj, path, todo)
    local i = 1
    local info = debug.getinfo(obj)
    info = "f:" .. info.short_src .. ":" .. info.linedefined
    while true do
        local n, v = debug.getupvalue(obj, i)
        if not n then
            break
        end
        if v then
            self:need_traverse(v, path_append(info .. "(up:" .. tostring(n) .. ")", path), todo)
        end
        i = i + 1
    end
end

local tracker
return function (output_filename, interval, age_threshold)
    if tracker ~= nil then return tracker end
    interval = interval or 60
    age_threshold = age_threshold or 600
    tracker = traverse.new(output_filename, age_threshold)
    gtimer {
        timeout = interval,
        call_now = false,
        autostart = true,
        callback = function ()
            tracker:scan()
        end
    }
    return tracker
end

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

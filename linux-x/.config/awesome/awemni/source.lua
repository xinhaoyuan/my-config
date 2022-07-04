-- Abstrations of lazy data pulling and transfromation from data streams.

local dummy = {
    fetch_async = function () end,
    reset = function () end,
}

local function from_array(array)
    return {
        fetch_async = function (self, index, cb)
            cb(index, array[index])
        end,
        reset = function (self, input, ready_cb)
            if ready_cb then ready_cb(#array) end
        end,
    }
end

local function id_func(x) return x end
local function true_func() return true end
local function ignore_self(f)
    return function (_self, ...) return f(...) end
end
local function filter(args)
    local upstream = args.upstream
    local pre_filter_cb = args.pre_filter_cb or id_func
    local filter_cb = args.filter_cb or true_func
    local post_filter_cb = args.post_filter_cb or id_func
    -- unfiltered pre/post-filter data 
    local upstream_data = {}
    local upstream_post_data = {}
    local upstream_sealed
    -- filtered slot data and/or callbacks
    local filtered_upstream_index
    local filtered
    local filtered_last_done
    local filtered_sealed
    local pending_filtered_indices
    local pending_requests
    -- source filtering state
    local last_ready_upstream_index
    local filter
    local ready_cb
    -- handlers
    local on_upstream_data
    local on_upstream_ready
    function on_upstream_data(upstream_index, element)
        if (upstream_sealed and upstream_index >= upstream_sealed) or upstream_data[upstream_index] then return end
        if element == nil then
            upstream_sealed = upstream_index
            if upstream_index > 1 and upstream_data[upstream_index - 1] == nil then return end
            filtered_upstream_index = upstream_index
            filtered_sealed = filtered_last_done + 1
            for k, v in pairs(pending_requests) do
                for _, cb in ipairs(v) do
                    cb(k, nil)
                end
                pending_requests[k] = nil
            end
            pending_filtered_indices = 0
            return
        end
        upstream_data[upstream_index] = element
        while pending_filtered_indices > 0 and upstream_data[filtered_upstream_index + 1] do
            filtered_upstream_index = filtered_upstream_index + 1
            if filter_cb(filter, upstream_data[filtered_upstream_index]) then
                filtered_last_done = filtered_last_done + 1
                filtered[filtered_last_done] = filtered_upstream_index
                if pending_requests[filtered_last_done] then
                    upstream_post_data[upstream_index] = post_filter_cb(upstream_data[upstream_index])
                    for _, cb in ipairs(pending_requests[filtered_last_done]) do
                        cb(filtered_last_done, upstream_post_data[upstream_index])
                    end
                    pending_requests[filtered_last_done] = nil
                    pending_filtered_indices = pending_filtered_indices - 1
                end
            end
        end
        on_upstream_ready()
        if filtered_upstream_index + 1 == upstream_sealed then
            filtered_upstream_index = upstream_sealed
            filtered_sealed = filtered_last_done + 1
            for k, v in pairs(pending_requests) do
                for _, cb in ipairs(v) do
                    cb(k, nil)
                end
                pending_requests[k] = nil
            end
            pending_filtered_indices = 0
        elseif pending_filtered_indices > 0 then
            return upstream:fetch_async(filtered_upstream_index + 1, on_upstream_data)
        end
    end
    function on_upstream_ready(upstream_index)
        if upstream_index then
            last_ready_upstream_index = upstream_index
        else
            upstream_index = last_ready_upstream_index
        end
        if upstream_index and ready_cb and filtered_upstream_index > 0 then
            return ready_cb(filtered_last_done / filtered_upstream_index * upstream_index)
        end
    end
    return {
        fetch_async = function (_self, index, cb)
            assert(index > 0)
            local upstream_index = filtered[index]
            if upstream_index then
                if not upstream_post_data[upstream_index] then
                    upstream_post_data[upstream_index] = post_filter_cb(upstream_data[upstream_index])
                end
                cb(index, upstream_post_data[upstream_index])
                return
            end
            if filtered_sealed and index >= filtered_sealed then cb(index, nil) end
            if pending_filtered_indices == 0 then
                while filtered_last_done < index and upstream_data[filtered_upstream_index + 1] do
                    filtered_upstream_index = filtered_upstream_index + 1
                    if filter_cb(filter, upstream_data[filtered_upstream_index]) then
                        filtered_last_done = filtered_last_done + 1
                        filtered[filtered_last_done] = filtered_upstream_index
                    end
                end
                on_upstream_ready()
                if filtered_last_done == index then
                    upstream_index = filtered[index]
                    if not upstream_post_data[upstream_index] then
                        upstream_post_data[upstream_index] = post_filter_cb(upstream_data[upstream_index])
                    end
                    cb(index, upstream_post_data[upstream_index])
                    return
                elseif filtered_upstream_index + 1 == upstream_sealed then
                    filter_sealed = filtered_last_done
                    cb(index, nil)
                    return
                end
            end
            local to_request_upstream = pending_filtered_indices == 0
            if pending_requests[index] == nil then
                pending_requests[index] = {}
                pending_filtered_indices = pending_filtered_indices + 1
            end
            table.insert(pending_requests[index], cb)
            if to_request_upstream then
                return upstream:fetch_async(filtered_upstream_index + 1, on_upstream_data)
            end
        end,
        reset = function (_self, input, new_ready_cb)
            filtered_upstream_index = 0
            filtered = {}
            filtered_last_done = 0
            filtered_sealed = nil
            pending_filtered_indices = 0
            pending_requests = {}
            filter = pre_filter_cb(input)
            last_ready_upstream_index = nil
            ready_cb = new_ready_cb
            upstream:reset(input, on_upstream_ready)
        end,
        on_upstream_data = ignore_self(on_upstream_data),
        on_upstream_next_data = function (_self, data)
            return on_upstream_data(#upstream_data + 1, data)
        end,
        on_upstream_ready = ignore_self(on_upstream_ready),
    }
end

local function concat(sources)
    assert(#sources > 0)
    local expansion_counts
    local slots -- member of each element: data, callbacks, source_index, source_element_index
    local slot_last_done
    local slot_last_requested
    local sealed
    return {
        fetch_async = function (self, index, cb)
            if slots[index] and slots[index].callbacks == nil then
                cb(index, slots[index].data)
                return
            end
            if sealed and index >= sealed then cb(index, nil) end
            if slot_last_requested == nil or slot_last_requested < index then slot_last_requested = index end
            if slots[index] == nil then slots[index] = {callbacks = {}} end
            table.insert(slots[index].callbacks, cb)
            if slots[slot_last_requested].callbacks == nil then
                -- No more request is on the fly.
                local index = (slot_last_done or 0) + 1
                local source_index, source_element_index
                if index == 1 then
                    source_index = 1
                    source_element_index = 1
                else
                    source_index = slots[index - 1].source_index
                    source_element_index = slots[index - 1].source_element_index + 1
                end
                local internal_handler
                function internal_handler(_source_element_index, data)
                    if data == nil and source_index < #sources then
                        source_index = source_index + 1
                        source_element_index = 1
                        return sources[source_index]:fetch_async(
                            source_element_index, internal_handler)
                    end
                    if slots[index] == nil then slots[index] = {} end
                    slots[index].data = data
                    slots[index].source_index = source_index
                    slots[index].source_element_index = source_element_index
                    if slots[index].callbacks then
                        for _, cb in ipairs(slots[index].callbacks) do
                            cb(index, data)
                        end
                        slots[index].callbacks = nil
                    end
                    slot_last_done = index
                    if data == nil then
                        sealed_index = index
                    elseif index < slot_last_requested then
                        index = index + 1
                        source_element_index = source_element_index + 1
                        return sources[source_index]:fetch_async(
                            source_element_index, internal_handler)
                    end
                end
                sources[source_index]:fetch_async(
                    source_element_index, internal_handler)
            end
        end,
        reset = function (self, input, ready_cb)
            expansion_counts = {}
            slots = {}
            slot_last_done = nil
            slot_last_requested = nil
            sealed = nil
            request_pending = false
            for i = 1, #sources do
                sources[i]:reset(
                    input, ready_cb and function (expansion_count)
                        expansion_counts[i] = expansion_count
                        local total = 0
                        for j = 1, #sources do
                            total = total + (expansion_counts[j] or 0)
                        end
                        ready_cb(total)
                    end)
            end
        end,
    }
end

return {
    dummy = dummy, 
    from_array = from_array,
    filter = filter,
    concat = concat,
}

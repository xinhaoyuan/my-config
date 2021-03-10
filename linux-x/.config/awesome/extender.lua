local m = {}

local function lexical_compare(a, b)
    for i = 1, #a do
        if a[i] ~= b[i] then
            return a[i] < b[i]
        end
    end
    return false
end

local function crop_area(a, sx, sy, ex, ey)
    if a.x < ex and a.x + a.width > sx and
        a.y < ey and a.y + a.height > sy then
        return {
            sx = math.max(a.x, sx),
            sy = math.max(a.y, sy),
            ex = math.min(a.x + a.width, ex),
            ey = math.min(a.y + a.height, ey),
        }
    else
        return nil
    end
end

-- Given a matrix w with size n*m, find all maximum rectangles without negative elements by
-- calling cb(x, y, w, h). At most n*m rectangles will be found.
local function find_max_rects(w, cb)
    local acc_row_h = {}
    for j = 1, #w do
        local inc_stack = {}
        local inc_stack_left = {}
        local inc_stack_len = 0
        for i = 1, #w[j] do
            if w[j][i] < 0 then
                acc_row_h[i] = 0
            elseif acc_row_h[i] then
                acc_row_h[i] = acc_row_h[i] + 1
            else
                acc_row_h[i] = 1
            end

            while inc_stack_len > 0 and acc_row_h[inc_stack[inc_stack_len]] > acc_row_h[i] do
                local h = acc_row_h[inc_stack[inc_stack_len]]
                local w = i - inc_stack_left[inc_stack_len] - 1
                local y = j - h + 1
                local x = inc_stack_left[inc_stack_len] + 1
                if w > 0 and h > 0 then
                    cb(x, y, w, h)
                end

                inc_stack[inc_stack_len] = nil
                inc_stack_left[inc_stack_len] = nil
                inc_stack_len = inc_stack_len - 1
            end

            inc_stack[inc_stack_len + 1] = i
            if inc_stack_len > 0 and acc_row_h[inc_stack[inc_stack_len]] == acc_row_h[i] then
                inc_stack_left[inc_stack_len + 1] = inc_stack_left[inc_stack_len]
            elseif inc_stack_len > 0 then
                inc_stack_left[inc_stack_len + 1] = inc_stack[inc_stack_len]
            else
                inc_stack_left[inc_stack_len + 1] = 0
            end
            inc_stack_len = inc_stack_len + 1
        end

        while inc_stack_len > 0 do
            local h = acc_row_h[inc_stack[inc_stack_len]]
            local w = #w[j] - inc_stack_left[inc_stack_len]
            local y = j - h + 1
            local x = inc_stack_left[inc_stack_len] + 1
            if w > 0 and h > 0 then
                cb(x, y, w, h)
            end

            inc_stack[inc_stack_len] = nil
            inc_stack_left[inc_stack_len] = nil
            inc_stack_len = inc_stack_len - 1
        end
    end
end

if not ... then
    local w = {
        { 1, -1, -1,  1},
        {-1,  1, -1,  1},
        { 1,  1, -1, -1},
        { 1,  1,  1,  1},
    }
    find_max_rects(w,
                   function(x, y, w, h)
                       print(x, y, w, h)
                   end
    )
end

function m.fit(workarea, orig_areas, options)
    options = options or {}
    -- 1. Discretize the points
    local sx, sy, ex, ey =
        workarea.x, workarea.y,
        workarea.x + workarea.width,
        workarea.y + workarea.height
    local areas = {{ sx = sx, ex = ex, sy = sy, ey = ey }}
    local x_values = {{sx, 0, #areas}, {ex, 0, #areas}}
    local y_values = {{sy, 0, #areas}, {ey, 0, #areas}}
    for i, a in ipairs(orig_areas) do
        local area = crop_area(a, sx, sy, ex, ey)
        if area then
            areas[#areas + 1] = area
            x_values[#x_values + 1] = {area.sx, 1, #areas}
            x_values[#x_values + 1] = {area.ex, -1, #areas}
            y_values[#y_values + 1] = {area.sy, 1, #areas}
            y_values[#y_values + 1] = {area.ey, -1, #areas}
        end
    end
    table.sort(x_values, lexical_compare)
    table.sort(y_values, lexical_compare)
    for i = 1, #x_values do
        -- This is not need to other areas, but for workarea and orig_area.
        if areas[x_values[i][3]].sx == x_values[i][1] and
            areas[x_values[i][3]].sxi == nil then
            areas[x_values[i][3]].sxi = i
        else
            areas[x_values[i][3]].exi = i
        end
    end
    for i = 1, #y_values do
        -- This is not need to other areas, but for workarea and orig_area.
        if areas[y_values[i][3]].sy == y_values[i][1] and
            areas[y_values[i][3]].syi == nil then
            areas[y_values[i][3]].syi = i
        else
            areas[y_values[i][3]].eyi = i
        end
    end
    -- 2. Build the matrix
    local m = {}
    for i = 1, #y_values - 1 do
        local row = {}
        m[#m + 1] = row
        local b_sy = y_values[i][1]
        local b_ey = y_values[i + 1][1]

        local cover_count = 0
        for j = 1, #x_values - 1 do
            if i < areas[x_values[j][3]].eyi and areas[x_values[j][3]].syi < i + 1 then
                cover_count = cover_count + x_values[j][2]
            end
            row[j] = cover_count == 0 and (y_values[i + 1][1] - y_values[i][1]) * (x_values[j + 1][1] - x_values[j][1]) or -1
        end
    end
    -- local acc = {}
    -- for i = 1, #y_values do
    --     acc[i] = {}
    --     for j = 1, #x_values do
    --         acc[i][j] = (i == 1 or j == 1) and 0 or
    --             acc[i - 1][j] + acc[i][j - 1] - acc[i - 1][j - 1] + m[i - 1][j - 1]
    --     end
    -- end
    -- 3.  the weighted maximum rectangle.
    local max_rect = nil
    find_max_rects(
        m,
        function (x, y, w, h)
            w = x_values[x + w][1] - x_values[x][1]
            h = y_values[y + h][1] - y_values[y][1]
            x = x_values[x][1]
            y = y_values[y][1]
            if options.min_size then
                if w < options.min_size or h < options.min_size then
                    return
                end
            end
            if max_rect ~= nil then
                if max_rect.width * max_rect.height > w * h then
                    return
                end
            else
                max_rect = {}
            end
            max_rect.x = x
            max_rect.y = y
            max_rect.width = w
            max_rect.height = h
        end
    )
    return max_rect
end

return m

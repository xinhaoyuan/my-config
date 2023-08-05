local function rows_iterator(str)
    return function(str, pos)
        if pos > #str then return nil, nil end
        local next_pos
        local row = {}
        local size = 0
        while next_pos == nil do
            local value
            local h
            local string_pieces
            while pos <= #str do
                h = str:sub(pos, pos)
                if h == '"' then
                    if value ~= nil then break end
                    local close_pos = str:find('"', pos + 1)
                    if close_pos == nil then
                        close_pos = #str + 1
                    end
                    local s = str:sub(pos + 1, close_pos - 1)
                    if string_pieces == nil then
                        string_pieces = {s}
                    else
                        table.insert(string_pieces, s)
                    end
                    pos = close_pos + 1
                elseif h == "," or h == '\n' or string_pieces ~= nil then
                    if h == "," and value == nil and string_pieces == nil then
                        value = ""
                    end
                    break
                else
                    local close_pos = str:find('[,"\n]', pos + 1)
                    if close_pos == nil then
                        close_pos = #str + 1
                    end
                    value = str:sub(pos, close_pos - 1)
                    pos = close_pos
                end
            end
            if value == nil and string_pieces ~= nil then
                value = table.concat(string_pieces, '"')
            end
            if value ~= nil or (h ~= '\n' and pos <= #str) then
                size = size + 1
                row[size] = value
            end
            if h == ',' then
                pos = pos + 1
            elseif h == '\n' then
                next_pos = pos + 1
            elseif pos > #str then
                next_pos = pos
            end
        end
        return next_pos, row
    end, str, 1
end

local function rows(str)
    local result = {}
    for _, row in rows_iterator(str) do
        table.insert(result, row)
    end
    return result
end

local function run_tests()
    local function check_rows_are_equal(a, b)
        if #a ~= #b then error(string.format("different # of rows: %d vs %d", #a, #b)) end
        for i = 1, #a do
            if #a[i] ~= #b[i] then error(string.format("different # of columns in row %d: %d vs %d", i, #a[i], #b[i])) end
            for j = 1, #a[i] do
                if a[i][j] ~= b[i][j] then
                    error(string.format("different value of row %d column %d: %s vs %s", i, j, tostring(a[i][j]), tostring(b[i][j])))
                end
            end
        end
    end

    -- Well-formed inputs.
    check_rows_are_equal(rows(""), {})
    check_rows_are_equal(rows("1,2,3"), {{"1", "2", "3"}})
    check_rows_are_equal(rows("1,2,3\n"), {{"1", "2", "3"}})
    check_rows_are_equal(rows("12,34,56"), {{"12", "34", "56"}})
    check_rows_are_equal(rows([[
1,2
3,4
]]),
                         {{"1", "2"}, {"3", "4"}})
    check_rows_are_equal(rows([[
1,"2,3",4
]]),
                         {{"1", "2,3", "4"}})
    check_rows_are_equal(rows([[
1,"2
3",4
]]),
                         {{"1", "2\n3", "4"}})
    check_rows_are_equal(rows([[
1,"2
3"",",4
]]),
                         {{"1", "2\n3\",", "4"}})
    check_rows_are_equal(rows(",\n"), {{""}})
    check_rows_are_equal(rows("1,\n"), {{"1"}})
    -- Bad but also toleratable inputs.
    check_rows_are_equal(rows([[1"2"3"4"5]]), {{"1", "2", "3", "4", "5"}})
    check_rows_are_equal(rows([["]]), {{""}})
    check_rows_are_equal(rows([["123]]), {{"123"}})
    check_rows_are_equal(rows("\n"), {{}})
    check_rows_are_equal(rows("\n\n"), {{},{}})
end

return {
    rows_iterator = rows_iterator,
    rows = rows,
    -- Test command: lua -e 'require("csv").__run_tests()'
    __run_tests = run_tests,
}

local function file_readable(path)
    local f = io.open(path, "r")
    if f then
        io.close(f)
        return true
    else
        return false
    end
end

local debug_file = os.getenv("HOME").."/.awesome_debug_trap"

return function(msg)
    if file_readable(debug_file) then
        print("Debug trap: "..msg..","..debug.traceback())
        print("Wait until "..debug_file.." is not readable...")
        repeat os.execute("sleep 1") until not file_readable(debug_file)
    else
        print("Ignored debug trap: "..msg..","..debug.traceback())
    end
end

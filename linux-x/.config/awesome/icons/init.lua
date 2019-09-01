local icons = {}
local local_path = debug.getinfo(1, "S").source:match("^@(.-)[^/]+$")

icons.cpu = local_path .. "cpu.svg" 
icons.ram = local_path .. "ram.svg"

return icons

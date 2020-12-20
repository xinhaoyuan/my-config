local icons = {}
local local_path = debug.getinfo(1, "S").source:match("^@(.-)[^/]+$")

icons.clock = local_path .. "clock.svg"
icons.cpu = local_path .. "cpu.svg" 
icons.ram = local_path .. "ram.svg"
icons.ethernet = local_path .. "ethernet.svg"
icons.disk = local_path .. "disk.svg"
icons.audio = local_path .. "audio.svg"
icons.note = local_path .. "note.svg"
icons.launcher = local_path .. "launcher.svg"
icons.browser = local_path .. "browser.svg"
icons.music = local_path .. "music.svg"
icons.fortune = local_path .. "fortune.svg"
icons.terminal = local_path .. "terminal.svg"
icons.file_manager = local_path .. "file_manager.svg"
icons.setup = local_path .. "setup.svg"
icons.battery_full = local_path .. "battery-full.svg"
icons.battery_half = local_path .. "battery-half.svg"
icons.dots = local_path .. "dots.svg"
icons.lock = local_path .. "lock.svg"
icons.sleep = local_path .. "sleep.svg"
icons.poweroff = local_path .. "poweroff.svg"


return icons

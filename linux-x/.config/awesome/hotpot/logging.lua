local mod = {
    LEVEL_DEBUG = -1,
    LEVEL_INFO = 0,
    LEVEL_WARNING = 1,
    LEVEL_ERROR = 2,
}

mod.level = mod.LEVEL_WARNING

function mod.add(level, ...)
    if level < mod.level then return end
    print(...)
end

function mod.debug(...)
    mod.add(mod.LEVEL_DEBUG, ...)
end

function mod.info(...)
    mod.add(mod.LEVEL_INFO, ...)
end

function mod.warning(...)
    mod.add(mod.LEVEL_WARNING, ...)
end

function mod.error(...)
    mod.add(mod.LEVEL_ERROR, ...)
end

return mod

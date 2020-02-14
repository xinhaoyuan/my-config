-- A pub-sub implementation in Awesome WM

local gtimer = require("gears.timer")
local gobject = require("gears.object")

local class = {}

function class:publish(topic, ...)
    self:emit_signal("topic::"..topic, ...)
end

function class:subscrible(topic, func)
    self:weak_connect_signal("topic::"..topic, func)
end

local pubsub = gobject { class = class }

return pubsub
    

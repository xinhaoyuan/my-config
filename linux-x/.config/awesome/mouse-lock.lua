local api = {
   awful = require("awful"),
   socket = require("socket"),  -- for getting time with high precision
   client = client,
   mouse = mouse,
}

local function between(s, e, c)
   if c < s then return s elseif c > e then return e else return c end
end

local config = {
   lock_interval = 0.25
}

api.client.connect_signal(
   "mouse::leave",
   function (c)
      now = api.socket.gettime()
      if c.timestamp_last_leaving == nil or
         now - c.timestamp_last_leaving > config.lock_interval / 2
      then
         c.timestamp_start_leaving = nil
      end
      
      if c.timestamp_start_leaving == nil then
         c.timestamp_start_leaving = now
      end
      c.timestamp_last_leaving = now

      if now - c.timestamp_start_leaving > config.lock_interval then
         c.timestamp_start_leaving = nil
      else
         mouse_pos = api.mouse.coords()
         api.mouse.coords(
            { x = between(c.x, c.x + c.width - 1, mouse_pos.x),
              y = between(c.y, c.y + c.height - 1, mouse_pos.y) })
      end
   end
)
   
return config

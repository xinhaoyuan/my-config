local awful        = require('awful')
local table        = table
local capi         = {
    client         = client,
    screen         = screen,
    awesome        = awesome,
}

local fh = {
   -- flag to not change the focus history when getting signals.
   -- this is useful when focus is changed temporarily (alt-tab cycling)
   ignore_focus_signal = false,
   -- The filter to ignore clients altogether (not to add to the history stack).
   -- The function should return true / the client if it's ok, nil otherwise.
   filter_focus_history = awful.client.focus.filter
}

local hist = {
    stack = {}
}

function hist.delete(c)
   -- XXX make it more efficient
   for k, v in ipairs(hist.stack) do
      if v == c then
         table.remove(hist.stack, k)
         break
      end
   end
end

function hist.add(c)
    if fh.filter_focus_history then
        if not fh.filter_focus_history(c) then
            return false
        end
    end
    -- Remove any existing entries from the stack.
    hist.delete(c)
    -- Record the client has latest focused
    table.insert(hist.stack, 1, c)
    return true
end

function fh.match_in_history(filters, fallback)
   for _, f in pairs(filters) do
      for _, c in ipairs(hist.stack) do
         if f(c) then
            return c
         end
      end
   end

   if fallback and #hist.stack > 0 then
      return hist.stack[0]
   else
      return nil
   end
end

function fh.add(c)
   return hist.add(c)
end

function fh.delete(c)
   return hist.delete(c)
end

function fh.get_stack()
   return hist.stack
end

-- connect to signals
capi.client.connect_signal("focus", function (c)
    if fh.ignore_focus_signal or capi.awesome.startup then
        return
    end
    hist.add(c)
end)

capi.client.connect_signal("manage", function (c)
    if ignore_focus_signal then
        return
    end
    hist.add(c)
end)

capi.client.connect_signal("unmanage", function (c)
    hist.delete(c)
end)

return fh

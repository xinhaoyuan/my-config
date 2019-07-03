-- the basic layout engine

local capi = {
   layout = require("awful.layout"),
   utils = require("my-utils"),
}

function region(x, y, w, h)
   return {x = x, y = y, width = w, height = h}
end

function do_arrange(p, priv)
   local wa = p.workarea
   local cls = p.clients
   local regions = priv.regions

   if regions == nil then
      regions = priv.compute_regions(p, priv)
   end

   priv.last_region_count = #regions

   for i, c in ipairs(cls) do
      if c.floating then
         print("Ignore client " .. tostring(c))
      else
         local region
         if c.machi_region == nil then
            c.machi_region = 1
            region = 1
         elseif c.machi_region > #regions then
            region = 1
         else
            region = c.machi_region
         end

         p.geometries[c] = {
            x = regions[region].x,
            y = regions[region].y,
            width = regions[region].width,
            height = regions[region].height,
         }

         print("Put client " .. tostring(c) .. " to region " .. region)

      end
   end
end

function create_layout(name, regions, data)
   local priv = {}

   if type(regions) == "function" then
      priv.compute_regions = regions
   elseif type(regions) == "table" then
      priv.regions = regions
   else
      priv.regions = {}
   end

   priv.data = data

   return {
      name = "machi[" .. name .. "]",
      arrange = function (p) do_arrange(p, priv) end,
      get_region_count = function () return priv.last_region_count end,
   }
end

function set_region(c, r)
   c.floating = false
   c.maximized = false
   c.fullscreen = false
   c.machi_region = r
   capi.layout.arrange(c.screen)
end

function cycle_region(c)
   layout = capi.layout.get(c.screen)
   count = layout.get_region_count and layout.get_region_count()
   if type(count) ~= "number" then count = 1 end
   current_region = c.machi_region or 1
   print(tostring(count) .. " - " .. tostring(current_region))
   if not capi.utils.is_tiling(c) then
      capi.utils.set_tiling(c)
   elseif current_region >= count then
      c.machi_region = 1
   else
      c.machi_region = current_region + 1
   end
   capi.layout.arrange(c.screen)
end

return
   {
      region = region,
      create_layout = create_layout,
      set_region = set_region,
      cycle_region = cycle_region,
   }

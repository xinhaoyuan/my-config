-- the basic layout engine

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

   for i, c in ipairs(cls) do
      if c.floating then
         print("Ignore client " .. tostring(c))
         goto continue
      end

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

      ::continue::
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
   }
end

return
   {
      region = region,
      create_layout = create_layout,
   }

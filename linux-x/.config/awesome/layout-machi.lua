-- the basic layout engine

local capi = {
   beautiful = require("beautiful"),
   wibox = require("wibox"),
   awful = require("awful"),
   screen = require("awful.screen"),
   layout = require("awful.layout"),
   utils = require("my-utils"),
   keygrabber = require("awful.keygrabber"),
   naughty = require("naughty"),
   gears = require("gears"),
   config = require("my-config"),
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

   if regions == nil or #regions == 0 then
      return
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
         elseif c.machi_region > #regions or c.machi_region <= 1 then
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

   local function set_regions(regions)
      priv.compute_regions = nil
      priv.regions = nil

      if type(regions) == "function" then
         priv.compute_regions = regions
      elseif type(regions) == "table" then
         priv.regions = regions
      end
   end

   set_regions(regions)
   priv.data = data

   return {
      name = "machi[" .. name .. "]",
      arrange = function (p) do_arrange(p, priv) end,
      get_region_count = function () return priv.last_region_count end,
      set_regions = set_regions,
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

function _area_tostring(wa)
   return "{x:" .. tostring(wa.x) .. ",y:" .. tostring(wa.y) .. ",w:" .. tostring(wa.width) .. ",h:" .. tostring(wa.height) .. "}"
end

function shrink_area_with_gap(a, gap)
   return { x = a.x + (a.bl and 0 or gap / 2), y = a.y + (a.bu and 0 or gap / 2),
            width = a.width - (a.bl and 0 or gap / 2) - (a.br and 0 or gap / 2),
            height = a.height - (a.bu and 0 or gap / 2) - (a.bd and 0 or gap / 2) }
end

local active_color = "#0000ff"
local open_color = "#ffffff"
local closed_color = "#000000"

function interactive_layout_edit()
   local screen = capi.screen.focused()
   local init_area = {
      x = screen.workarea.x,
      y = screen.workarea.y,
      width = screen.workarea.width,
      height = screen.workarea.height,
      border = 15,
      depth = 0,
      -- we do not want to rely on bitop
      bl = true, br = true, bu = true, bd = true,
   }
   local kg
   local closed_areas = {}
   local open_areas = {init_area}
   print("interactive layout editing starts")
   local split = nil
   local ratio_lu = nil
   local ratio_rd = nil
   local max_depth = 2
   local infobox = capi.wibox({
         x = screen.workarea.x,
         y = screen.workarea.y,
         width = screen.workarea.width,
         height = screen.workarea.height,
         bg = "#ffffff00",
         opacity = 1,
         ontop = true
   })
   infobox.visible = true
   local current_cmd = ""

   local function draw_info(context, cr, width, height)
      cr:set_source_rgba(0, 0, 0, 0)
      cr:rectangle(0, 0, width, height)
      cr:fill()

      local msg, ext

      for i, a in ipairs(closed_areas) do
         local gap = capi.config.border_gap * capi.config.widget_scale_factor
         local sa = shrink_area_with_gap(a, gap)
         cr:set_source(capi.gears.color(closed_color .. "77"))
         cr:rectangle(sa.x, sa.y, sa.width, sa.height)
         cr:fill()

         cr:select_font_face("Sans", "normal", "normal")
         cr:set_font_size(30)
         cr:set_font_face(cr:get_font_face())
         msg = tostring(i)
         ext = cr:text_extents(msg)
         cr:set_source_rgba(1, 1, 1, 1)
         cr:move_to(sa.x + sa.width / 2 - ext.width / 2 - ext.x_bearing, sa.y + sa.height / 2 - ext.height / 2 - ext.y_bearing)
         cr:show_text(msg)
      end

      for i, a in ipairs(open_areas) do
         local gap = capi.config.border_gap * capi.config.widget_scale_factor
         local sa = shrink_area_with_gap(a, gap)
         cr:set_source(capi.gears.color((i == #open_areas and active_color or open_color) .. "77") )
         cr:rectangle(sa.x, sa.y, sa.width, sa.height)
         cr:fill()
      end

      cr:select_font_face("Sans", "normal", "normal")
      cr:set_font_size(60)
      cr:set_font_face(cr:get_font_face())
      msg = current_cmd
      ext = cr:text_extents(msg)
      cr:move_to(width / 2 - ext.width / 2 - ext.x_bearing, height / 2 - ext.height / 2 - ext.y_bearing)
      cr:text_path(msg)
      cr:set_source_rgba(1, 1, 1, 1)
      cr:fill()
      cr:move_to(width / 2 - ext.width / 2 - ext.x_bearing, height / 2 - ext.height / 2 - ext.y_bearing)
      cr:text_path(msg)
      cr:set_source_rgba(0, 0, 0, 1)
      cr:set_line_width(2.0)
      cr:stroke()
   end

   local function refresh_areas()
      print("closed areas:")
      for i, a in ipairs(closed_areas) do
         print("  " .. _area_tostring(a))
      end
      print("open areas:")
      for i, a in ipairs(open_areas) do
         print("  " .. _area_tostring(a))
      end
      infobox.bgimage = draw_info
   end

   local function handle_split(key, alt)
      if ratio_lu == nil then ratio_lu = 1 end
      if ratio_rd == nil then ratio_rd = 1 end

      if alt then
         local tmp = ratio_lu
         ratio_lu = ratio_rd
         ratio_rd = tmp
      end

      local a = open_areas[#open_areas]
      table.remove(open_areas, #open_areas)
      local lu, rd

      print("split " .. key .. " " .. tostring(alt) .. " " .. _area_tostring(a))

      if key == "h" then
         lu = {
            x = a.x, y = a.y,
            width = a.width / (ratio_lu + ratio_rd) * ratio_lu, height = a.height,
            depth = a.depth + 1,
            bl = a.bl, bu = a.bu, bd = a.bd,
         }
         rd = {
            x = a.x + lu.width, y = a.y,
            width = a.width - lu.width, height = a.height,
            depth = a.depth + 1,
            br = a.br, bu = a.bu, bd = a.bd,
         }
      elseif key == "v" then
         lu = {
            x = a.x, y = a.y,
            width = a.width, height = a.height / (ratio_lu + ratio_rd) * ratio_lu,
            depth = a.depth + 1,
            bl = a.bl, br = a.br, bu = a.bu,
         }
         rd = {
            x = a.x, y = a.y + lu.height,
            width = a.width, height = a.height - lu.height,
            depth = a.depth + 1,
            bl = a.bl, br = a.br, bd = a.bd,
         }
      end
      open_areas[#open_areas + 1] = rd
      open_areas[#open_areas + 1] = lu

      ratio_lu = nil
      ratio_rd = nil

      refresh_areas()
   end

   local function cleanup()
      infobox.visible = false
   end

   local function push_area()
      closed_areas[#closed_areas + 1] = open_areas[#open_areas]
      table.remove(open_areas, #open_areas)
      infobox.bgimage = draw_info
   end

   refresh_areas()

   kg = keygrabber.run(function (mod, key, event)
         if event == "release" then
            return
         end

         local to_exit = false
         local to_apply = false

         if key == "h" then
            current_cmd = current_cmd .. "h"
            handle_split("h", false)
         elseif key == "H" then
            current_cmd = current_cmd .. "H"
            handle_split("h", true)
         elseif key == "v" then
            current_cmd = current_cmd .. "v"
            handle_split("v", false)
         elseif key == "V" then
            current_cmd = current_cmd .. "V"
            handle_split("v", true)
         elseif key == " " then
            current_cmd = current_cmd .. "_"
            if ratio_lu ~= nil then
               max_depth = ratio_lu
               ratio_lu = nil
               ratio_rd = nil
            else
               push_area()
               if #open_areas == 0 then
                  to_exit = true
                  to_apply = true
               end
            end
         elseif key == "Return" then
            while #open_areas > 0 do
               push_area()
            end
            to_exit = true
            to_apply = true
         elseif tonumber(key) ~= nil then
            current_cmd = current_cmd .. key
            local v = tonumber(key)
            if v > 0 then
               if ratio_lu == nil then
                  ratio_lu = v
               elseif ratio_rd == nil then
                  ratio_rd = v
               end
            end
         elseif key == "Escape" then
            to_exit = true
         end

         while #open_areas > 0 and open_areas[#open_areas].depth >= max_depth do
            push_area()
         end

         if #open_areas == 0 then
            to_exit = true
            to_apply = true
         end

         if to_exit then
            print("interactive layout editing ends")
            if to_apply then
               layout = capi.layout.get(screen)
               if layout.set_regions then
                  local gap = capi.config.border_gap * capi.config.widget_scale_factor
                  local areas_with_gap = {}
                  for _, a in ipairs(closed_areas) do
                     areas_with_gap[#areas_with_gap + 1] = shrink_area_with_gap(a, gap)
                  end
                  layout.set_regions(areas_with_gap)
                  capi.layout.arrange(screen)
               end
               capi.gears.timer{
                  timeout = 1,
                  autostart = true,
                  singleshot = true,
                  callback = cleanup
               }
            else
               cleanup()
            end
            keygrabber.stop(kg)
            return
         end
   end)
end

return
   {
      region = region,
      create_layout = create_layout,
      set_region = set_region,
      cycle_region = cycle_region,
      interactive_layout_edit = interactive_layout_edit,
   }

local api = {
   beautiful  = require("beautiful"),
   wibox      = require("wibox"),
   awful      = require("awful"),
   screen     = require("awful.screen"),
   layout     = require("awful.layout"),
   keygrabber = require("awful.keygrabber"),
   naughty    = require("naughty"),
   gears      = require("gears"),
   lgi        = require("lgi"),
   dpi        = require("beautiful.xresources").apply_dpi,
}

local function new(gravity, offset_x, offset_y, width, height, draw)

   local data = {
      client = nil,
      gravity = gravity,
      offset_x = offset_x,
      offset_y = offset_y,
      width = width,
      height = height,
      draw = draw,
      widget = api.wibox({
            x = 0,
            y = 0,
            width = 0,
            height = 0,
            bg = "#00000000",
            opacity = 1,
            ontop = true,
      }),
   }

   data.widget.bgimage = draw

   local function update_geometry()
      if data.client == nil then return end

      if gravity == "N" then
         data.widget.x = data.client.x + data.client.width / 2 - data.width / 2
         data.widget.y = data.client.y + data.offset_y
      elseif gravity == "S" then
         data.widget.x = data.client.x + data.client.width / 2 - data.width / 2
         data.widget.y = data.client.y + data.client.height - data.offset_y - data.height
      elseif gravity == "W" then
         data.widget.x = data.client.x + data.offset_x
         data.widget.y = data.client.y + data.client.height / 2 - data.height / 2
      elseif gravity == "E" then
         data.widget.x = data.client.x + data.client.width - data.offset_x - data.width
         data.widget.y = data.client.y + data.client.height / 2 - data.height / 2
      elseif gravity == "NW" then
         data.widget.x = data.client.x + data.offset_x
         data.widget.y = data.client.y + data.offset_y
      elseif gravity == "NE" then
         data.widget.x = data.client.x + data.client.width - data.offset_x - data.width
         data.widget.y = data.client.y + data.offset_y
      elseif gravity == "SW" then
         data.widget.x = data.client.x + data.offset_x
         data.widget.y = data.client.y + data.client.height - data.offset_y - data.height
      elseif gravity == "SE" then
         data.widget.x = data.client.x + data.client.width - data.offset_x - data.width
         data.widget.y = data.client.y + data.client.height - data.offset_y - data.height
      end
   end

   local function update_draw()
      if data.client == nil then data.widget.visible = false; return end
      if data.draw ~= nil then data.bgimage = data.draw end
      if not data.widget.visible then data.widget.visible = true end
   end

   local function geometry_callback(c, context, args)
      if c ~= data.client then return end
      update_geometry()
   end

   local function focus_callback(c)
      data.client = c

      update_geometry()
      update_draw()
   end

   local function unfocus_callback(c)
      if data.client == c then
         data.client = nil
      end

      update_geometry()
   end

   return data

end

-- Yet Another Simple Switcher
--   Author: Xinhao Yuan <xinhaoyuan@gmail.com>

local api = {
   gears     = require("gears"),
   beautiful = require("beautiful"),
   wibox     = require("wibox"),
   awful     = require("awful"),
   lgi       = require("lgi"),
   dpi       = require("beautiful.xresources").apply_dpi,
}

local focus_timestamp = 0
local function update_focus_timestamp(c)
   if c == nil then return end
   if c.focus_timestamp ~= nil and
      c.focus_timestamp > focus_timestamp
   then
      focus_timestamp = c.focus_timestamp
   end
   focus_timestamp = focus_timestamp + 1
   c.focus_timestamp = focus_timestamp
end

-- For avoiding accidentally updating ordering during switching
local focus_timestamp_update_lock = false
client.connect_signal(
   "focus",
   function (c)
      if focus_timestamp_update_lock then return end
      update_focus_timestamp(c)
   end
)

local function min(a, b)
   if a < b then return a else return b end
end

local function max(a, b)
   if a < b then return b else return a end
end

local function with_alpha(col, alpha)
   local r, g, b
   _, r, g, b, _ = col:get_rgba()
   return api.lgi.cairo.SolidPattern.create_rgba(r, g, b, alpha)
end

local function create(config)
   local function filter(c)
      return c:isvisible() and api.awful.client.focus.filter(c)
   end

   if config and config.filter ~= nil then
      filter = config.filter
   end

   local same_screen = true
   if config and config.same_screen ~= nil then
      same_screen = config.same_screen
   end

   local to_release = { ["Alt_L"] = true }

   local function start(screen)
      local tablist_font_desc = api.beautiful.get_merged_font(
         api.beautiful.mono_font or api.beautiful.font, api.dpi(10))
      local font_color = with_alpha(api.gears.color(api.beautiful.fg_normal), 1)
      local font_color_hl = with_alpha(api.gears.color(api.beautiful.fg_focus), 1)
      local label_size = api.dpi(30)
      local border_color = with_alpha(api.gears.color(api.beautiful.border_focus), 0.75)
      local fill_color = with_alpha(api.gears.color(api.beautiful.bg_normal), 0.75)
      local fill_color_hl = with_alpha(api.gears.color(api.beautiful.bg_focus), 1)

      if screen == nil then
         screen = api.awful.screen.focused()
      end

      local start_x = screen.workarea.x
      local start_y = screen.workarea.y

      local panel = api.wibox({
            screen = screen,
            x = screen.workarea.x,
            y = screen.workarea.y,
            width = screen.workarea.width,
            height = screen.workarea.height,
            bg = "#00000000",
            opacity = 1,
            ontop = true
      })

      local tablist = nil

      local function ensure_tablist()
         if tablist == nil then
            tablist = {}
            for c in api.awful.client.iterate(filter, nil, same_screen and screen or nil) do
               tablist[#tablist + 1] = c
            end
            table.sort(tablist, function (a, b) return a.focus_timestamp ~= nil and b.focus_timestamp ~= nil and a.focus_timestamp > b.focus_timestamp end)
            tablist_index = 1
         end
      end

      local function draw_info(context, cr, width, height)
         ensure_tablist()

         cr:set_source_rgba(0, 0, 0, 0)
         cr:rectangle(0, 0, width, height)
         cr:fill()

         local msg, ext
         local pl = api.lgi.Pango.Layout.create(cr)
         pl:set_font_description(tablist_font_desc)

         local vpadding = api.dpi(10)
         local info_height = vpadding
         local info_width = 2 * vpadding
         local exts = {}

         for index, tc in ipairs(tablist) do
            local label = tc.name
            pl:set_text(label)
            local w, h
            w, h = pl:get_size()
            w = w / api.lgi.Pango.SCALE
            h = h / api.lgi.Pango.SCALE
            local ext = { width = w, height = h, x_bearing = 0, y_bearing = 0 }
            exts[#exts + 1] = ext
            info_height = info_height + ext.height + vpadding
            info_width = max(info_width, w + 2 * vpadding)
         end

         local x_offset = width / 2
         local y_offset = height / 2 - info_height / 2 + vpadding

         cr:rectangle((width - info_width) / 2, y_offset - vpadding - start_y, info_width, info_height)
         cr:set_source(fill_color)
         cr:fill()

         for index, tc in ipairs(tablist) do
            local label = tc.name
            local ext = exts[index]
            if index == tablist_index then
               cr:rectangle(x_offset - ext.width / 2 - vpadding / 2, y_offset - vpadding / 2, ext.width + vpadding, ext.height + vpadding)
               cr:set_source(fill_color_hl)
               cr:fill()
               pl:set_text(label)
               cr:move_to(x_offset - ext.width / 2 - ext.x_bearing, y_offset - ext.y_bearing)
               cr:set_source(font_color_hl)
               cr:show_layout(pl)
            else
               pl:set_text(label)
               cr:move_to(x_offset - ext.width / 2 - ext.x_bearing, y_offset - ext.y_bearing)
               cr:set_source(font_color)
               cr:show_layout(pl)
            end

            y_offset = y_offset + ext.height + vpadding
         end
      end

      local function switch()
         if #tablist > 0 then
            tablist_index = tablist_index % #tablist + 1
            c = tablist[tablist_index]
            c:emit_signal("request::activate", "mouse.move", {raise=false})
            c:raise()
         end
      end

      ensure_tablist()
      if #tablist < 2 then
         return
      end

      panel.visible = true
      panel.bgimage = draw_info
      focus_timestamp_update_lock = true

      switch()

      local kg = nil
      local function stop()
         update_focus_timestamp(tablist[tablist_index])
         focus_timestamp_update_lock = false
         panel.visible = false
         if kg ~= nil then
            api.awful.keygrabber.stop(kg)
            kg = nil
         end
      end

      kg = api.awful.keygrabber.run(
         function (mod, key, event)
            if event == "release" then
               if to_release[key] then
                  stop()
               end
            elseif key == "Tab" then
               switch()
               panel.bgimage = draw_info
            end
            return true
         end
      )
   end

   return { start = start }
end

return {
   create = create,
   default = create(),
}

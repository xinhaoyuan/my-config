-- Yet Another Minimal Switcher
--   Author: Xinhao Yuan <xinhaoyuan@gmail.com>

local api = {
   gears     = require("gears"),
   beautiful = require("beautiful"),
   wibox     = require("wibox"),
   awful     = require("awful"),
   fts       = require("focus-timestamp"),
   lgi       = require("lgi"),
   dpi       = require("beautiful.xresources").apply_dpi,
}

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

local function activate(c)
   c:emit_signal("request::activate", "mouse.move", {raise=false})
end

local function create(config)
   local function filter(c)
      return c:isvisible() and api.awful.client.focus.filter(c)
   end

   if config and config.filter ~= nil then
      filter = config.filter
   end

   local same_screen = false
   if config and config.same_screen ~= nil then
      same_screen = config.same_screen
   end

   local to_release = { }
   if config and config.release_key ~= nil then
      to_release[config.release_key] = true
   else
      to_release["Alt_L"] = true
   end

   local switch_key
   if config and config.switch_key ~= nil then
      switch_key = config.switch_key
   else
      switch_key = "Tab"
   end

   local function start(screen)
      local tablist_font_desc = api.beautiful.get_merged_font(
         api.beautiful.mono_font or api.beautiful.font, api.dpi(10))
      local font_color = with_alpha(api.gears.color(api.beautiful.fg_normal), 1)
      local font_color_hl = with_alpha(api.gears.color(api.beautiful.fg_focus), 1)
      local label_size = api.dpi(30)
      local border_color = with_alpha(api.gears.color(api.beautiful.border_focus), 0.85)
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
            ontop = true,
            type = "dock",
      })

      local tablist = nil
      local tablist_index = nil

      local function initialize()
         tablist = {}
         for c in api.awful.client.iterate(filter, nil, same_screen and screen or nil) do
            tablist[#tablist + 1] = c
         end
         table.sort(
            tablist,
            function (a, b)
               return api.fts.get(a) > api.fts.get(b)
            end
         )

         for i = #tablist, 1, -1 do
            local c = tablist[i]
            c.saved_layer_info = {c.ontop, c.above, c.below}
            c.ontop = false
            c.above = false
            c.below = false
            if c.saved_layer_info[1] or c.saved_layer_info[2] then
               c:raise()
            elseif c.saved_layer_info[3] then
               c:lower()
            end
         end

         tablist_index = 1
      end

      local function finalize()
         for i = 1, #tablist do
            local c = tablist[i]
            c.ontop = c.saved_layer_info[1]
            c.above = c.saved_layer_info[2]
            c.below = c.saved_layer_info[3]
            c.saved_layer_info = nil
         end
         activate(tablist[tablist_index])
         tablist[tablist_index]:raise()
      end

      local function draw_info(context, cr, width, height)
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

         cr:rectangle((width - info_width) / 2, y_offset - vpadding, info_width, info_height)
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
            tablist[tablist_index].ontop = false
            tablist_index = tablist_index % #tablist + 1
            tablist[tablist_index].ontop = true
            activate(tablist[tablist_index])
         end
      end

      initialize()
      if #tablist < 2 then
         finalize()
         return
      end

      api.awful.client.focus.history.disable_tracking()

      switch()

      local kg = nil

      local function stop()
         -- At this moment, tablist[tablist_index] is already focused.
         -- We do not want to trigger the focus event by focus-out-and-focus-in.
         -- So we just manually update the history info instead.
         api.fts.update(tablist[tablist_index])
         api.awful.client.focus.history.add(tablist[tablist_index])
         api.awful.client.focus.history.enable_tracking()
         panel.visible = false
         if kg ~= nil then
            api.awful.keygrabber.stop(kg)
            kg = nil
         end
         finalize()
      end

      panel.bgimage = draw_info
      panel.visible = true

      kg = api.awful.keygrabber.run(
         function (mod, key, event)
            if event == "release" then
               if to_release[key] then
                  stop()
               end
            elseif key == switch_key then
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

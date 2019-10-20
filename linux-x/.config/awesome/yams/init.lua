-- Yet Another Minimal Switcher
--   Author: Xinhao Yuan <xinhaoyuan@gmail.com>

local api = {
   client    = client,
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
   -- the default filter will get all focusable client with any selected tags
   local function default_filter(c)
      if not api.awful.client.focus.filter(c) then return false end
      for _, t in ipairs(c:tags()) do
         if t.selected then
            return true
         end
      end
      return false
   end

   config = config or {}
   if config.same_screen == nil then
       config.same_screen = true
   end

   if config.filter == nil then
       config.filter = default_filter
   end

   config.to_release = config.to_release or { ["Alt_L"] = true }
   if config.switch_key == nil then
       config.switch_key = "Tab"
   end

   if config.opacity_other == nil then
       config.opacity_other = 0.5
   end

   if config.opacity_selected == nil then
       config.opacity_selected = 1
   end

   if config.panel == nil then
       config.panel = true
   end
   
   local function start(screen)
      local tablist_font_desc = api.beautiful.get_merged_font(
         api.beautiful.font, api.dpi(10))
      local font_color = with_alpha(api.gears.color(api.beautiful.fg_normal), 1)
      local font_color_hl = with_alpha(api.gears.color(api.beautiful.fg_focus), 1)
      local label_size = api.dpi(30)
      local fill_color = with_alpha(api.gears.color(api.beautiful.bg_normal), 0.85)
      local fill_color_hl = with_alpha(api.gears.color(api.beautiful.bg_focus), 1)

      if screen == nil then
         if api.client.focus then
            screen = api.client.focus.screen
         else
            screen = api.awful.screen.focused()
         end
      end

      local start_x = screen.workarea.x
      local start_y = screen.workarea.y

      local panel = nil

      if config.panel then
          panel = api.wibox({
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
      end

      local tablist = nil
      local tablist_index = nil

      -- filter out invalid clients, but will not add new ones in.
      local function maintain_tablist()
         local j = 0
         for i = 1, #tablist do
            if tablist[i].valid then
               j = j + 1
               tablist[j] = tablist[i]
            elseif i <= tablist_index and tablist_index > 1 then
               tablist_index = tablist_index - 1
            end
         end
         for i = #tablist, j + 1, -1 do
            table.remove(tablist, i)
         end
      end

      local function initialize()
         tablist = {}
         for c in api.awful.client.iterate(config.filter, nil, config.same_screen and screen or nil) do
            tablist[#tablist + 1] = c
         end
         table.sort(
            tablist,
            function (a, b)
               -- prioritize non-minimized client
               if a.minimized ~= b.minimized then
                  return b.minimized
               end
               return api.fts.get(a) > api.fts.get(b)
            end
         )

         for i = #tablist, 1, -1 do
            local c = tablist[i]
            c.saved = {ontop = c.ontop, above = c.above, below = c.below, minimized = c.minimized, opacity = c.opacity}
            c.ontop = false
            c.above = false
            c.below = false
            c.opacity = config.opacity_other
            -- if c.saved.ontop or c.saved.above then
            --    c:raise()
            -- elseif c.saved.below then
            --    c:lower()
            -- end
         end

         tablist_index = 1
      end

      local function finalize()
         for i = 1, #tablist do
            local c = tablist[i]
            c.ontop = c.saved.ontop
            c.above = c.saved.above
            c.below = c.saved.below
            c.minimized = c.saved.minimized
            c.opacity = c.saved.opacity
            c.saved = nil
         end

         if #tablist > 0 then
            local c = tablist[tablist_index]
            if c.minimized then
               c.minimized = false
            end
            activate(c)
            c:raise()
         end
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
         local labels = {}

         for index, tc in ipairs(tablist) do
            if tc.valid then
               local label = tc.name or "<Untitled>"
               pl:set_text(label)
               local w, h
               w, h = pl:get_size()
               w = w / api.lgi.Pango.SCALE
               h = h / api.lgi.Pango.SCALE
               local ext = { width = w, height = h, x_bearing = 0, y_bearing = 0 }
               table.insert(exts, ext)
               table.insert(labels, label)
               info_height = info_height + ext.height + vpadding
               info_width = max(info_width, w + 2 * vpadding)
            end
         end

         local x_offset = width / 2
         local y_offset = height / 2 - info_height / 2 + vpadding

         cr:rectangle((width - info_width) / 2, y_offset - vpadding, info_width, info_height)
         cr:set_source(fill_color)
         cr:fill()

         for index, label in ipairs(labels) do
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
         maintain_tablist()
         if #tablist > 0 then
            local cc = tablist[tablist_index]
            cc.above = false
            cc.opacity = config.opacity_other
            cc.minimized = cc.saved.minimized
            tablist_index = tablist_index % #tablist + 1
            cc = tablist[tablist_index]
            cc.opacity = config.opacity_selected
            cc.minimized = false
            cc.above = true
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
         maintain_tablist()
         if #tablist > 0 then
            api.fts.update(tablist[tablist_index])
            api.awful.client.focus.history.add(tablist[tablist_index])
         end
         api.awful.client.focus.history.enable_tracking()
         if panel then
             panel.visible = false
         end
         if kg ~= nil then
            api.awful.keygrabber.stop(kg)
            kg = nil
         end
         finalize()
      end

      if panel then
          panel.bgimage = draw_info
          panel.visible = true
      end

      kg = api.awful.keygrabber.run(
         function (mod, key, event)
            if event == "release" then
               if config.to_release[key] then
                  stop()
               end
            elseif key == config.switch_key then
                switch()
                if panel then
                    panel.bgimage = draw_info
                end
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

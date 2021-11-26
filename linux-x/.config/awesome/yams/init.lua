-- Yet Another Minimal Switcher
--   Author: Xinhao Yuan <xinhaoyuan@gmail.com>

local capi = {
    client = client,
}

local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local fts = require("hotpot").focus_timestamp
local lgi = require("lgi")
local dpi = require("beautiful.xresources").apply_dpi
local border = require("border-theme")
local unpack = unpack or table.unpack

local function with_alpha(col, alpha)
    local r, g, b
    _, r, g, b, _ = col:get_rgba()
    return lgi.cairo.SolidPattern.create_rgba(r, g, b, alpha)
end

local function activate_client(c)
    -- Do not raise the window to preserve the stacking order.
    c:emit_signal("request::activate", "switch", {raise=false})
end

-- the default filter will get all focusable client with any selected tags
local function default_filter(c)
    if not awful.client.focus.filter(c) then return false end
    if c.cgroup ~= nil and c.cgroup.current_client ~= c then return false end
    for _, t in ipairs(c:tags()) do
        if t.selected then
            return true
        end
    end
    return false
end

local function default_clients(screen)
    local result = {}
    for c in awful.client.iterate(default_filter, nil, screen) do
        result[#result + 1] = c
    end
    return result
end

local function create(config)
    config = config or {}

    if config.source == nil then
        config.source = function(args)
        end
    end

    config.keys = config.keys or { ["Alt_L"] = "release_to_exit", ["Alt_R"] = "release_to_exit", ["Tab"] = "switch", ["q"] = "switch_back" }

    if config.opacity_other == nil then
        config.opacity_other = 0.5
    end

    if config.opacity_selected == nil then
        config.opacity_selected = 1
    end

    if config.panel == nil then
        config.panel = true
    end

    local function start(args)
        local tablist_font_desc = beautiful.get_merged_font(
            beautiful.font, dpi(10))
        local font_color = with_alpha(gears.color(beautiful.fg_normal), 1)
        local font_color_hl = with_alpha(gears.color(beautiful.fg_focus), 1)
        local label_size = dpi(30)
        local fill_color = with_alpha(gears.color(beautiful.bg_normal), 0.85)
        local fill_color_hl = with_alpha(gears.color(beautiful.bg_focus), 1)

        if args.screen == nil then
            if capi.client.focus then
                args.screen = capi.client.focus.screen
            else
                args.screen = awful.screen.focused()
            end
        end
        local screen = args.screen
        local start_x = screen.workarea.x
        local start_y = screen.workarea.y
        local current_focus = capi.client.focus
        local client_compare = function (a, b)
            if a == current_focus then return false elseif b == current_focus then return true end
            -- prioritize non-minimized client
            if a.minimized ~= b.minimized then
                return b.minimized
            end
            return fts.get(a) > fts.get(b)
        end
        local saved_client_compare = screen.client_compare

        local panel = nil

        if config.panel then
            panel = wibox({
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
            if args.clients then
                tablist = {unpack(args.clients)}
            else
                tablist = default_clients(args.screen)
            end
            table.sort(tablist, client_compare)

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
                if c == current_focus then tablist_index = i end
            end
            -- Set to the end since we'll switch once at the beginning.
            if tablist_index == nil then tablist_index = #tablist end
        end

        local function finalize()
            for i = 1, #tablist do
                local c = tablist[i]
                -- To work well with cgroup, minimized needs to be restored first.
                c.minimized = c.saved.minimized
                c.ontop = c.saved.ontop
                c.above = c.saved.above
                c.below = c.saved.below
                c.opacity = c.saved.opacity
                c.saved = nil
            end

            if #tablist > 0 then
                local c = tablist[tablist_index]
                if c.minimized then
                    c.minimized = false
                end
                activate_client(c)
                c:raise()
            end

            if screen.client_compare ~= saved_client_compare then
                screen.client_compare = saved_client_compare
                capi.client.emit_signal("list")
            end
        end

        local function draw_info(context, cr, width, height)
            cr:set_source_rgba(0, 0, 0, 0)
            cr:paint()

            local msg, ext
            local pl = lgi.Pango.Layout.create(cr)
            pl:set_font_description(tablist_font_desc)

            local padding = beautiful.border_width
            local inner_padding = padding
            local panel_height = 0
            local panel_width = 0
            local exts = {}
            local labels = {}

            for index, tc in ipairs(tablist) do
                if tc.valid then
                    local label = tc.name or "<Untitled>"
                    pl:set_text(label)
                    local w, h
                    w, h = pl:get_size()
                    w = w / lgi.Pango.SCALE
                    h = h / lgi.Pango.SCALE
                    local ext = { width = w, height = h, x_bearing = 0, y_bearing = 0 }
                    table.insert(exts, ext)
                    table.insert(labels, label)
                    panel_height = panel_height + ext.height + 2 * inner_padding
                    panel_width = math.max(panel_width, w + 2 * inner_padding)
                end
            end

            local panel_x = width / 2 - panel_width / 2 - padding
            local panel_y = height / 2 - panel_height / 2 - padding

            cr:save()
            cr:translate(panel_x, panel_y)
            cr:rectangle(0, 0, panel_width + padding * 2, panel_height + padding * 2)
            cr:clip()
            cr:set_source(fill_color)
            cr:paint()
            border:draw({ color = beautiful.border_focus }, cr, panel_width + padding * 2, panel_height + padding * 2, border.directions { "top", "bottom", "left", "right" })
            cr:restore()

            local label_x = panel_x + padding
            local label_y = panel_y + padding
            for index, label in ipairs(labels) do
                local ext = exts[index]
                if index == tablist_index then
                    cr:rectangle(label_x, label_y, panel_width, ext.height + inner_padding * 2)
                    cr:set_source(fill_color_hl)
                    cr:fill()
                    pl:set_text(label)
                    cr:move_to(label_x + (panel_width - ext.width) / 2 - ext.x_bearing, label_y + inner_padding - ext.y_bearing)
                    cr:set_source(font_color_hl)
                    cr:show_layout(pl)
                else
                    pl:set_text(label)
                    cr:move_to(label_x + (panel_width - ext.width) / 2 - ext.x_bearing, label_y + inner_padding - ext.y_bearing)
                    cr:set_source(font_color)
                    cr:show_layout(pl)
                end

                label_y = label_y + ext.height + inner_padding * 2
            end
        end

        local function switch(forward)
            maintain_tablist()
            if #tablist > 0 then
                local cc = tablist[tablist_index]
                cc.above = false
                cc.opacity = config.opacity_other
                cc.minimized = cc.saved.minimized
                tablist_index = forward and tablist_index % #tablist + 1 or (tablist_index + #tablist - 2) % #tablist + 1
                cc = tablist[tablist_index]
                cc.opacity = config.opacity_selected
                cc.minimized = false
                cc.above = true
                activate_client(tablist[tablist_index])
            end
        end

        initialize()
        if #tablist < 2 then
            finalize()
            return
        end

        awful.client.focus.history.disable_tracking()
        screen.client_compare = client_compare
        capi.client.emit_signal("list")

        switch(true)

        local kg = nil

        local function stop()
            -- At this moment, tablist[tablist_index] is already focused.
            -- We do not want to trigger the focus event by focus-out-and-focus-in.
            -- So we just manually update the history info instead.
            maintain_tablist()
            if #tablist > 0 then
                fts.update(tablist[tablist_index])
                awful.client.focus.history.add(tablist[tablist_index])
            end
            awful.client.focus.history.enable_tracking()
            if panel then
                panel.visible = false
            end
            if kg ~= nil then
                awful.keygrabber.stop(kg)
                kg = nil
            end
            finalize()
        end

        if panel then
            panel.bgimage = draw_info
            panel.visible = true
        end

        kg = awful.keygrabber.run(
            function (mod, key, event)
                local action = config.keys[key]
                if event == "release" then
                    if action == "release_to_exit" then
                        stop()
                    end
                elseif action == "switch" or action == "switch_back" then
                    switch(action == "switch")
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

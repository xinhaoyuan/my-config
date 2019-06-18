local be = require("beautiful")
local gshape = require("gears.shape")
local wi = require("wibox")
local hidpi = os.getenv("HIDPI") and #os.getenv("HIDPI") > 0

be.init("/usr/share/awesome/themes/default/theme.lua")
be.tasklist_shape = function(cr, w, h)
   offset = h / 4
   cr:move_to(0, 0)
   cr:line_to(w, 0)
   cr:line_to(w - offset, h)
   cr:line_to(offset, h)
   cr:close_path()
end
-- be.tasklist_shape_focus = gshape.powerline

local config = {
   hidpi = hidpi,
   widget_scale_factor = hidpi and 2 or 1,
   font_normal = hidpi and "Input 10" or "Terminus 10",
   font_mono = hidpi and "Input 10" or "Terminus 10",
   bar_fontsize = 9,
   bar_height = 16,
   tag_filter = function (name)
      return name ~= "STICKY"
   end,
   cmd_terminal = "urxvt",
}

config.tasklist_template = {
   {
      {
         {
            {
               id     = "icon_role",
               widget = wi.widget.imagebox,
            },
            id = "icon_margin_role",
            margins = 2 * config.widget_scale_factor,
            widget  = wi.container.margin,
         },
         {
            id     = "text_role",
            widget = wi.widget.textbox,
         },
         layout = wi.layout.fixed.horizontal,
      },
      id = "text_margin_role",
      left  = 5 * config.widget_scale_factor,
      right = 5 * config.widget_scale_factor,
      widget = wi.container.margin
   },
   id     = "background_role",
   widget = wi.container.background,
}

return config

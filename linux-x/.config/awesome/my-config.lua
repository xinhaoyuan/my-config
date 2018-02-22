local hidpi = os.getenv("HIDPI") and #os.getenv("HIDPI") > 0
local config = {
   hidpi = hidpi,
   widget_scale_factor = hidpi and 2 or 1,
   fontname_text = "Sans",
   fontname_mono = "Input",
   font_scale_factor = 1,
   tag_filter = function (name)
      return name ~= "STICKY"
   end
}

return config

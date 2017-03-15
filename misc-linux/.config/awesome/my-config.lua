local config = {
   widget_scale_factor = 2,
   font_scale_factor = 1,
   tag_filter = function (name)
      return name ~= "STICKY"
   end
}

return config

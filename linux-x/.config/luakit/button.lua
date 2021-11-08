local get_theme = require("lousy.theme").get

local _M = {}

function _M.new(args)
    assert(type(args) == "table")

    local ret = {}
    local theme = get_theme()
    local ebox = widget{ type = "eventbox" }
    local label_widget = widget{ type = "label" }
    label_widget.text = args.text
    label_widget.margin_left = 6
    label_widget.margin_right = 6
    local button_layout = widget{ type = "hbox" }
    button_layout.margin_left = 5
    button_layout.homogeneous = false
    if args.icon or args.icon_text then
        local icon_widget
        if args.icon then 
            icon_widget = widget{ type = "image" }
            icon_widget:filename(args.icon)
        else
            icon_widget = widget{ type = "label" }
            icon_widget.text = args.icon_text
            icon_widget.margin = 0
            icon_widget.align = { h = "center", v = "center" }
        end
        if args.icon_bg then
            icon_widget.bg = args.icon_bg
        end
        icon_widget.min_size = { h = 32, w = 32 }
        label_widget.align = { v = "center" }
        button_layout:pack(icon_widget)
    else
        label_widget.align = { h = "center", v = "center" }
    end
    button_layout:pack(label_widget, { expand = true, fill = true })
    label_widget.fg = theme.tab_fg
    ebox.bg = theme.tab_bg
    ebox.child = button_layout

    ebox:add_signal(
        "mouse-enter", function ()
            label_widget.fg = theme.tab_hover_fg or theme.tab_fg
            ebox.bg = theme.tab_hover_bg
        end)
    ebox:add_signal(
        "mouse-leave", function ()
            label_widget.fg = theme.tab_fg
            ebox.bg = theme.tab_bg
        end)
    ebox:add_signal(
        "button-release", function ()
            args.on_click(ret)
        end)
    
    ret.widget = ebox
    return ret
end

return setmetatable(_M, { __call = function(_, ...) return _M.new(...) end })

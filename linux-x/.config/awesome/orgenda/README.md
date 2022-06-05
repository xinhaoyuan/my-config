# Orgenda

An AwesomeWM integration of org-mode.

Author: Xinhao Yuan <xinhaoyuan@gmail.com>

## Features

  - Read progress (TODO/DONE), priorities ([#A/B/C]), tags (:some:tags:), and timestamps.
  - Supports updating progress and priority in the UI.
  - Auto-refreshs using `fswatch`.

## Usage

  1. In `rc.lua`, initialize the module by calling `init{...}` function with `.org` file paths. E.g.

    ```lua
    require("organda").init{
        files = {
            os.getenv("HOME").."/todo.org"
        }
    }
    ```

  2. Create an organda widget that list the todo items. E.g.

    ```lua
    local wibox = require("wibox")
    local awful = require("awful")
    local orgenda = require("orgenda")
    local todos_popup = awful.popup{
        widget = {
            orgenda.widget{},
            width = 600,
            widget = wibox.container.constraint,
        },
        placement = awful.placement.centered,
        ontop = true,
        visible = false,
    }

    awful.keyboard.append_global_keybindings{
        awful.key({ "Mod4" }, "t", function ()
                     if todos_popup.visible then
                         todos_popup.visible = false
                     else
                         todos_popup.screen = mouse.screen
                         todos_popup.visible = true
                     end
                  end),
    }
    ```

## Configuration/customization

Check `init.lua` for the available and default config.

`orgenda.widget` also allows a number of customization options in the argument of the constructor.

## Stability/compatibility

I use it in my daily driver running AwesomeWM 4.3-git.

## Dependencies

  - fswatch: Watches changes in org files.

## License

MIT license, see LICENSE file.

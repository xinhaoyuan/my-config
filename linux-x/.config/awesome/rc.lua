-- awesome v4 config
-- Author: Xinhao Yuan (xinhaoyuan@gmail.com)

-- Remember certain information after client is detached but before the object is gone.
-- Need to do this before everything, so that signal handler fires before standard ones.

local capi = {
   client = client,
}

capi.client.connect_signal(
   "unmanage",
   function (c)
      c.tomb_floating = c.floating
      c.tomb_class = c.class
      c.tomb_pid = c.pid
   end
)

-- Print the error message to .xsession-errors.
awesome.connect_signal(
   "debug::error",
   function (msg)
      print(msg)
   end
)

local HOME_DIR = os.getenv("HOME")

os.execute(HOME_DIR .. "/.xdesktoprc.awesome")

require("config")
require("my-autofocus")
require("my-menu")

require("gears.timer").delayed_call(
   function ()
      if capi.client.focus then
         capi.client.focus:emit_signal("request::activate", "mouse.move", {raise=true})
      end
   end
)

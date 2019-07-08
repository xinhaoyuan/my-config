#!/usr/bin/env python3

import subprocess as sp
import re
import math
import sys

dpi = None
dpy_info = sp.check_output("xrandr | grep ' connected '", shell = True).decode("utf-8")
for line in dpy_info.split('\n'):
    if "primary" in line:
        m = re.search("([0-9]+)mm x ([0-9]+)mm", line)
        if not m:
            continue
        dev_width = int(m.group(1))
        dev_height = int(m.group(2))

        m = re.search("([0-9]+)x([0-9]+)\\+[0-9]+\\+[0-9]+", line)
        if not m:
            continue
        res_width = int(m.group(1))
        res_height = int(m.group(2))

        sys.stderr.write(
            "Info: {} {} {} {}\n".format(dev_width, dev_height, res_width, res_height))

        dpi_w = res_width / (dev_width / 25.4)
        dpi = math.floor(dpi_w / 96) * 96

if dpi is not None:
    scaling = dpi / 96.0
    icon_size = int(24 * scaling)
    sys.stderr.write("Set dpi to {}\n".format(dpi))
    sys.stderr.write("Set scaling to {:0.1f}\n".format(scaling))
    sys.stderr.write("Set icon_size to {}\n".format(icon_size))
    
    sp.call("echo 'Xft.dpi:{}' | xrdb -merge".format(dpi), shell = True)
    sp.call("echo 'hidpi.scaling:{:0.1f}' | xrdb -merge".format(scaling), shell = True)
    sp.call("echo 'Xcursor*size:{}' | xrdb -merge".format(icon_size), shell = True)

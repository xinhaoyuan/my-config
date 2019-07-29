#!/usr/bin/env python3

import subprocess as sp
import re
import math
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--fallback-dpi", dest = "fallback_dpi", type = int)
parser.add_argument("--max-dpi", dest = "max_dpi", type = int)
parser.add_argument("--min-dpi", dest = "min_dpi", type = int)
args = parser.parse_args()

dpi = None
dpy_info = sp.check_output("xrandr | grep ' connected '", shell = True).decode("utf-8")
for line in dpy_info.split('\n'):
    m = re.search("([0-9]+)mm x ([0-9]+)mm", line)
    if not m:
        continue
    dev_width = int(m.group(1))
    dev_height = int(m.group(2))

    if dev_width == 0:
        continue

    m = re.search("([0-9]+)x([0-9]+)\\+[0-9]+\\+[0-9]+", line)
    if not m:
        continue
    res_width = int(m.group(1))
    res_height = int(m.group(2))

    sys.stderr.write(
        "Info: {} {} {} {}\n".format(dev_width, dev_height, res_width, res_height))

    dpi_w = res_width / (dev_width / 25.4)
    sys.stderr.write("Info dpi_w: {}".format(dpi_w))
    cur_dpi = round(dpi_w / 48) * 48
    if dpi is None or dpi < cur_dpi:
        dpi = cur_dpi

if dpi is None:
    dpi = args.fallback_dpi

if dpi is not None and args.max_dpi is not None:
    dpi = min(args.max_dpi, dpi)

if dpi is not None and args.min_dpi is not None:
    dpi = max(args.min_dpi, dpi)

if dpi is not None:
    scaling = dpi / 96.0
    icon_size = int(24 * scaling)
    sys.stderr.write("Set dpi to {}\n".format(dpi))
    sys.stderr.write("Set scaling to {:0.1f}\n".format(scaling))
    sys.stderr.write("Set icon_size to {}\n".format(icon_size))

    sp.call("echo '*dpi:{}' | xrdb -merge".format(dpi), shell = True)
    sp.call("echo 'Xft.dpi:{}' | xrdb -merge".format(dpi), shell = True)
    sp.call("echo 'hidpi.scaling:{:0.1f}' | xrdb -merge".format(scaling), shell = True)
    sp.call("echo 'Xcursor*size:{}' | xrdb -merge".format(icon_size), shell = True)

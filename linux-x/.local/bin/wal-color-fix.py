#!/usr/bin/env python3

from colour import Color
import os, sys
import json

LUMINANCE_PAIR_DIFF = 0.1
LUMINANCE_END_DIFF = 0.1

def enforce_liminance_diff(a, b, target_diff, *, mode="MUTUAL"):
    if mode == "MUTUAL" and a.luminance > b.luminance:
        a, b = b, a
    l_delta = b.luminance - a.luminance
    if l_delta < target_diff:
        if mode == "MUTUAL":
            a.luminance = max(0, a.luminance - (target_diff - l_delta) / 2)
            b.luminance = min(1, b.luminance + (target_diff - l_delta) / 2)
        elif mode == "A":
            a.luminance = max(0, a.luminance - (target_diff - l_delta))
        elif mode == "B":
            b.luminance = min(1, b.luminance + (target_diff - l_delta))

def fix_colors(colors):
    fg = Color(colors["special"]["foreground"])
    bg = Color(colors["special"]["background"])
    cursor = Color(fg)
    c = []
    for i in range(16):
        c.append(Color(colors["colors"]["color{}".format(i)]))

    light_mode = bg.luminance > fg.luminance
    enforce_liminance_diff(fg, bg, 0.5)

    for i in range(8):
        if c[i].luminance > c[i + 8].luminance:
            c[i], c[i + 8] = c[i + 8], c[i]
    if c[0].luminance > c[7].luminance:
        c[0], c[7] = c[7], c[0]
    if c[8].luminance > c[15].luminance:
        c[8], c[15] = c[15], c[8]
    for i in range(1, 7):
        if light_mode:
            enforce_liminance_diff(c[i], bg, LUMINANCE_END_DIFF, mode="A")
            enforce_liminance_diff(fg, c[i], LUMINANCE_END_DIFF, mode="B")
            enforce_liminance_diff(c[i + 8], bg, LUMINANCE_END_DIFF, mode="A")
            enforce_liminance_diff(fg, c[i + 8], LUMINANCE_END_DIFF, mode="B")
            pass
        else:
            enforce_liminance_diff(bg, c[i], LUMINANCE_END_DIFF, mode="B")
            enforce_liminance_diff(c[i], fg, LUMINANCE_END_DIFF, mode="A")
            enforce_liminance_diff(bg, c[i + 8], LUMINANCE_END_DIFF, mode="B")
            enforce_liminance_diff(c[i + 8], fg, LUMINANCE_END_DIFF, mode="A")
    for i in range(8):
        enforce_liminance_diff(c[i], c[i + 8], LUMINANCE_PAIR_DIFF)

    colors["special"]["foreground"] = fg.hex_l
    colors["special"]["background"] = bg.hex_l
    colors["special"]["cursor"] = cursor.hex_l
    for i in range(16):
        colors["colors"]["color{}".format(i)] = c[i].hex_l
    return colors

def main():
    cache_home = os.getenv("XDG_CACHE_HOME")
    if cache_home is None:
        cache_home = os.getenv("HOME") + "/.cache"
    colors_path = cache_home + "/wal/colors.json"
    with open(colors_path, "r") as colors_input:
        colors = json.load(colors_input)
        fix_colors(colors)
    with open(colors_path, "w") as colors_output:
        json.dump(colors, colors_output)

if __name__ == "__main__":
    main()

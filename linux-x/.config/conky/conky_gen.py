import subprocess
import os
import sys

def gen_bar(options):
    lines = [
        "background no",
        "cpu_avg_samples 2",
        "net_avg_samples 2",
        "out_to_console no",
        "use_xft yes",
        "xftfont {0}".format(options["font"]),
        "xftalpha 0.5",
        "update_interval 1",
        "own_window yes",
        "own_window_colour 000000",
        "own_window_argb_visual yes",
        "own_window_argb_value 128",
        "own_window_class Conky",
        "own_window_type override",
        "double_buffer yes",
        "draw_shades no",
        "draw_outline no",
        "draw_borders no",
        "stippled_borders 8",
        "default_color white",
        "default_shade_color black",
        "default_outline_color black",
        "alignment top_left",
        "minimum_size  {0} {1}".format(options["screen-width"], 16 * options["scale-factor"]),
        "maximum_width {0}".format(options["screen-width"]),
        "maximum_height {0}".format(16 * options["scale-factor"]),
        "border_inner_margin 0",
        "border_outer_margin 0",
        "border_width 0",
        "padding 0",
        "gap_x 0",
        "gap_y 0",
        "use_spacer left",
        "no_buffers yes",
        "uppercase no",
        "pad_percents 2",
        "TEXT",
        "${color orange}Proc ${color} $running_processes/$processes, ${color orange}Load${color} ${loadavg 3} \\",
        "${color orange}Mem ${color} $memperc%, ${color orange}Swap ${color} $swapperc%\\"]
    if "mpd" in options and options["mpd"]:
        lines.extend([
            "${{alignc {0}}}${{if_mpd_playing}}\\".format(-200 * options["scale-factor"]),
            "${{font {0}}}${{mpd_artist}} - ${{mpd_title}}${{font}}\\".format(options["mpd_font"]),
            "${endif}\\"])
    lines.extend([
        "${alignr}\\",
        "${color orange}Mail${color}[${texeci 60 gm-query $HOME/.config/conky/gm-cookies.txt}] \\",
        "${color orange}Bat${color}[${battery_short}]\\",
        ""])
    return lines

def gen_details(options):
    lines = [
        "background no",
        "cpu_avg_samples 2",
        "net_avg_samples 2",
        "out_to_console no",
        "use_xft yes",
        "xftfont {0}".format(options["font"]),
        "xftalpha 0.5",
        # Update interval in seconds
        "update_interval 1"];

    lines.extend([
        # Create own window instead of using desktop (required in nautilus)
        "own_window yes",
        # "own_window_transparent yes",
        "own_window_colour 000000",
        "own_window_argb_visual yes",
        "own_window_argb_value 128",
        "own_window_class Conky",
        "own_window_type dock",
        # "own_window_hints below,skip_taskbar,skip_pager",
        # "minimum_size 800 40",
        # "maximum_width 800"
        ])

    lines.extend([
        # Use double buffering (reduces flicker, may not work for everyone)
        "double_buffer yes",
        # Draw shades?
        "draw_shades no",
        # Draw outlines?
        "draw_outline no",
        # Draw borders around text
        "draw_borders no",
        "border_inner_margin {0}".format(10 * options["scale-factor"]),
        "border_outer_margin 0",
        "border_width 0",
        # Stippled borders?
        "stippled_borders 8",
        # Default colors and also border colors
        "default_color white",
        "default_shade_color black",
        "default_outline_color black",
        "alignment top_right"
    ])

    lines.extend([
        # Gap between borders of screen and text
        "gap_x {0}".format(10 * options["scale-factor"]),
        "gap_y {0}".format(10 * options["scale-factor"] + 16 * options["scale-factor"]),
        # Add spaces to keep things from moving about?  This only affects certain objects.
        "use_spacer left",
        # Subtract file system buffers from used memory?
        "no_buffers yes",
        # set to yes if you want all text to be in uppercase
        "uppercase no",
        "pad_percents 2"
    ])

    lines.append("TEXT")

    cpu_count = int(subprocess.check_output("nproc"))

    # print cpu stats
    sys.stderr.write("Found {0} cpus".format(cpu_count))
    lines.extend([
        "${alignc}==== ${color orange}CPU${color} ====",
        ""
    ])
    first = True
    for cpu in range(0, cpu_count):
        if first:
            lines.append("${{cpu cpu{0}}}%,${{freq_g {0}}}\\".format(cpu + 1))
            first = False
        else:
            lines.append("|${{cpu cpu{0}}}%,${{freq_g {0}}}\\".format(cpu + 1))
        if cpu % 4 == 3:
            lines.append("")
            first = True

    lines.append("")

    lines.append("")
    lines.append("${alignc}==== ${color orange}NET${color} ====")
    lines.append("")
    for if_name in os.listdir("/sys/class/net"):
        if if_name == "lo":
            continue
        wireless = os.access("/sys/class/net/{0}/wireless".format(if_name), os.R_OK)
        lines.append("${alignc}\\")
        if wireless:
            lines.append("{0}: ${{addr {0}}} - ${{wireless_link_qual_perc {0}}}%".format(if_name))
        else:
            lines.append("{0}: ${{addr {0}}}".format(if_name))
        lines.append("${{alignc}}${{upspeed {0}}}/${{downspeed {0}}}".format(if_name))
        lines.append("")

    return lines

def main(args):
    hidpi = os.getenv("HIDPI") and len(os.getenv("HIDPI")) > 0
    try:
        screen_width = int(subprocess.check_output("xrandr | grep -o -e 'connected primary [0-9]*' | awk '{ printf $3 }'", shell = True))
    except:
	screen_width = 1920
    options = { "scale-factor" : 2 if hidpi else 1,
                "screen-width" : screen_width,
                "font" : "Input:size=9" if hidpi else "Terminus:size=10",
                "mpd" : True, "mpd_font" : "Vera Sans YuanTi Mono:size=9" }

    if args[0] == "details":
        lines = gen_details(options)
    elif args[0] == "bar":
        lines = gen_bar(options)
    else:
        lines = []
    sys.stdout.write("\n".join(lines))

if __name__ == "__main__":
    main(sys.argv[1:])

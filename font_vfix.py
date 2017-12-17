#!/usr/bin/env python

import os, sys, re
import argparse
import fontforge as ff

def main():
    parser = argparse.ArgumentParser("Fix vertical metrics of fonts")
    parser.add_argument("-d", "--adjust", dest = "adjust", action = "store",
                        type = int,
                        help = "do the fixing with delta")
    parser.add_argument("-r", "--rename", dest = "rename", action = "store",
                        type = str,
                        help = "rename the font family name")
    parser.add_argument("--hack-fix-win-metrics", dest = "hack_fix_win", action = "store_true",
                        help = "set winascent and windescent to original ascent and descent")
    parser.add_argument("fonts", metavar = "font", type = str, nargs = "+", help = "list of font files")
    args = parser.parse_args()

    base_dir = os.getcwd()
    for file_name in args.fonts:
        if not re.match(".*\.ttf", file_name, flags = re.IGNORECASE):
            continue
        file_path = os.path.join(base_dir, file_name)
        try:
            font = ff.open(file_path)
            if args.adjust is not None:
                sys.stdout.write("Working on \"{0}\": ".format(file_name))
                if args.rename is not None:
                    new_familyname = args.rename.format(name = font.familyname)
                    new_fontname = font.fontname.replace(font.familyname.replace(" ", ""), new_familyname.replace(" ", "", 1))
                    new_fullname = font.fullname.replace(font.familyname, new_familyname, 1)
                    sys.stdout.write("Rename {0} => {1}\n".format(
                        (font.fontname, font.familyname, font.fullname),
                        (new_fontname, new_familyname, new_fullname)))
                    font.fontname = new_fontname
                    font.familyname = new_familyname
                    font.fullname = new_fullname
                font.hhea_ascent = font.hhea_ascent + args.adjust
                font.hhea_descent = font.hhea_descent + args.adjust
                font.os2_typoascent = font.os2_typoascent + args.adjust
                font.os2_typodescent = font.os2_typodescent + args.adjust
                font.ascent = font.ascent + args.adjust
                font.descent = font.descent - args.adjust # this is negated
                if args.hack_fix_win:
                    font.os2_winascent = font.ascent
                    font.os2_windescent = font.descent
                font.generate(file_path)
                font.close()
                sys.stdout.write("Done.")
            else:
                sys.stdout.write("Inspecting \"{0}\": ".format(file_name))
                sys.stdout.write(
                    "{0};{1};{2};{3}, {4}".format(
                        (font.fontname, font.familyname, font.fullname),
                        (font.hhea_ascent, font.hhea_descent),
                        (font.os2_typoascent, font.os2_typodescent),
                        (font.os2_winascent, font.os2_windescent),
                        (font.ascent, font.descent)))
        except Exception as x:
            print("Got exception {0}".format(x))
        finally:
            sys.stdout.write("\n")

if __name__ == "__main__":
    main()

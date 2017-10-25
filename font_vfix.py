#!/usr/bin/env python

import os, sys, re
import argparse
import fontforge as ff

def main():
    parser = argparse.ArgumentParser("Fix vertical metrics of fonts")
    parser.add_argument("-d", "--adjust", dest = "adjust", action = "store",
                        type = int,
                        help = "do the fixing with delta")
    parser.add_argument("fonts", metavar = "font", type = str, nargs = "+", help = "list of font files")
    args = parser.parse_args()

    base_dir = os.getcwd()
    for file_name in args.fonts:
        if not re.match("consola.*\.ttf", file_name, flags = re.IGNORECASE):
            continue
        file_path = os.path.join(base_dir, file_name)
        try:
            font = ff.open(file_path)
            if args.adjust is not None:
                sys.stdout.write("Working on \"{0}\": ".format(file_name))
                font.hhea_ascent = font.hhea_ascent + args.adjust
                font.hhea_descent = font.hhea_descent + args.adjust
                font.os2_typoascent = font.os2_typoascent + args.adjust
                font.os2_typodescent = font.os2_typodescent + args.adjust
                font.ascent = font.ascent + args.adjust
                font.descent = font.descent - args.adjust # this is negated
                font.generate(file_path)
                font.close()
                sys.stdout.write("Done.")
            else:
                sys.stdout.write("Inspecting \"{0}\": ".format(file_name))
                sys.stdout.write(
                    "{0} {1} {2} {3} {4} {5}".format(
                        font.hhea_ascent,
                        font.hhea_descent,
                        font.os2_typoascent,
                        font.os2_typodescent,
                        font.ascent,
                        font.descent))
        finally:
            sys.stdout.write("\n")

if __name__ == "__main__":
    main()

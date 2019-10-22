#!/usr/bin/env python3

"""
Converts a xsane multipage project directory to a pdf file.

Author: Xinhao Yuan
Last updated: 2019-10-21
"""

import subprocess
import argparse
import sys, os
import logging
import tempfile
import re

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__file__)

parser = argparse.ArgumentParser()
parser.add_argument("-d", help = "the directory containing the multipage project", dest="input_dir", required=True)
parser.add_argument("-o", help = "the output pdf file", dest="output", required=True)
args = parser.parse_args()

if not os.path.isdir(args.input_dir):
    logger.error("wrong directory") 
    sys.exit(-1)

with tempfile.TemporaryDirectory() as workspace:
    # the first two lines are dummy
    line_to_ignore_count = 2
    convert_output_cmd = ["convert"]
    for line in open(os.path.join(args.input_dir, "xsane-multipage-list")):
        if line_to_ignore_count > 0:
            line_to_ignore_count -= 1
            continue

        filename = line.strip()
        m = re.match(r".*-([0-9]*)\.pnm", filename)
        if not m:
            logger.warning("{} does not match filename pattern".format(filename))
            continue

        converted_page = os.path.join(workspace, "converted-{}.jpg".format(m.group(1)))
        convert_page_cmd = ["convert", os.path.join(args.input_dir, filename), converted_page]
        convert_output_cmd.append(converted_page)
        logger.info("execute: {}".format(convert_page_cmd))
        subprocess.check_call(convert_page_cmd)
    convert_output_cmd.extend(["-density", "300x300", args.output])
    logger.info("execute: {}".format(convert_output_cmd))
    subprocess.check_call(convert_output_cmd)

#!/usr/bin/env python3

# A simple utility program that fetches xkcd.com comics and recolors them.

import requests
import json
import random
import argparse
import subprocess
import tempfile
import sys

def get_xkcd_comic(output_path, comic_num = None):
    """
    Fetches an xkcd comic of a given number to a specified file path.
      Number 0 means the current comic; None means random.
    """
    if comic_num is None or comic_num <= 0:
        resp = requests.get("https://xkcd.com/info.0.json")
        if resp.status_code != 200:
            raise Exception("Querying current comic info returns {}".format(resp.status_code))
        current_info = json.loads(resp.content)
        if comic_num is None:
            comic_num = random.randint(1, current_info["num"])
    if comic_num > 0:
        resp = requests.get("https://xkcd.com/{}/info.0.json".format(comic_num))
        if resp.status_code != 200:
            raise Exception("Querying random comic {} info returns {}".format(comic_num, resp.status_code))
        current_info = json.loads(resp.content)
    sys.stderr.write("Fetching xkcd #{}\n".format(current_info["num"]))
    img_url = current_info["img"]
    img_resp = requests.get(img_url)
    if img_resp.status_code != 200:
        raise Exception("Fetch comic image {} returns {}".format(img_url, img_resp.status_code))
    img_type = img_resp.headers.get("content-type")
    if not img_type.startswith("image/"):
        raise Exception("Fetched comic has the type of {} instead of image/...".format(img_type))
    img_ext = img_type[6:]
    if img_ext not in ["png", "jpeg", "gif"]:
        raise Exception("Fetched comic has the unsupported ext of {}.".format(img_ext))
    if output_path is None:
        with tempfile.NamedTemporaryFile(suffix="."+img_ext) as tmp:
            output_path = tmp.name
    with open(output_path, "wb") as output_file:
        output_file.write(img_resp.content)
    return output_path


parser = argparse.ArgumentParser()
parser.add_argument("-n", dest="num", type=int, help="The comic number to fetch. Random by default, and 0 means the current comic.")
parser.add_argument("-o", dest="output", type=str, help="If set, will write to file path. Otherwise a temporary-like path will be printed.")
parser.add_argument("--recolor", dest="recolor", type=str, help="#FFFGGGG-#BBBGGG to recolor the fetched image. Requires ImageMagick.")
args = parser.parse_args()

output = get_xkcd_comic(args.output, comic_num = args.num)
if args.recolor is not None:
    colors = args.recolor.split("-")
    subprocess.check_call([
        "convert", output, "+profile", "icc",
        "-size", "1x6", "gradient:{}-{}".format(colors[0], colors[1]),
        "-clut", output])
sys.stdout.write(output)

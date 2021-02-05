#!/usr/bin/env python3

import sys
import os, re
import subprocess

args = sys.argv
if len(args) < 4:
    sys.stderr.write("{} [device-pattern] [property-pattern] values ...\n".format(args[0]))
    sys.exit(1)
    pass

dev_pat = args[1]
prop_pat = args[2]

dev_matched = []
for line in subprocess.check_output("xinput").decode().split("\n"):
    m = re.search("^\s*(.*)id=([0-9]+)", line)
    if not m:
        continue
    dev_name = m.group(1).strip()
    dev_id = int(m.group(2))
    m = re.search(dev_pat, line)
    if not m:
        continue
    dev_matched.append((line, dev_id, dev_name))
    pass

if len(dev_matched) != 1:
    sys.stderr.write("Find {} matching dev, expecting exact match.\n{}\n".format(len(dev_matched), "\n".join([str(i) for i in dev_matched])))
    sys.exit(1)
    pass

prop_matched = []
for line in subprocess.check_output(["xinput", "list-props", str(dev_matched[0][1])]).decode().split("\n"):
    m = re.search("^\s*(.*)\\(([0-9]+)\\):", line)
    if not m:
        continue
    prop_name = m.group(1).strip()
    prop_id = int(m.group(2))
    m = re.search(prop_pat, prop_name)
    if not m:
        continue
    prop_matched.append((line, prop_id, prop_name))
    pass

if len(prop_matched) != 1:
    sys.stderr.write("Find {} matching props, expecting exact match.\n{}\n".format(len(prop_matched), "\n".join([str(i) for i in prop_matched])))
    sys.exit(1)
    pass

cmd = ["xinput", "set-prop", str(dev_matched[0][1]), str(prop_matched[0][1])]
cmd.extend(args[3:])
subprocess.check_call(cmd)

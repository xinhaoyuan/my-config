#!/usr/bin/env python3

import sys
import argparse
import re

parser = argparse.ArgumentParser()
parser.add_argument('task_files', nargs='+')
args = parser.parse_args()

tag_re = re.compile('#TASK:(.*)$')

tasks = {}

for task_file in args.task_files:
    for line in open(task_file):
        m = re.search(tag_re, line)
        if not m:
            continue
        tasks[m.group(1)] = line
        pass
    pass

for line in sys.stdin:
    m = re.search(tag_re, line)
    if not m:
        sys.stdout.write(line)
        continue
    if m.group(1) in tasks:
        sys.stdout.write(tasks[m.group(1)])
        del tasks[m.group(1)]
    else:
        sys.stdout.write(line)
        pass
    pass

for task in tasks:
    sys.stdout.write(tasks[task])


#!/bin/sh

xinput-set-prop.py Touchpad 'Tapping Enabled$' 1
xinput-set-prop.py 'Goodix.*pointer' Transformation 0 1 0 -1 0 1 0 0 1

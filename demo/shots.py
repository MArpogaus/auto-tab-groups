#!/usr/bin/env python3
"""Cut the tab bar out of the frame that shots.el exported.

Usage: python3 demo/shots.py img/tab-bar.png
"""
import sys

from PIL import Image

HEIGHT_FILE = "/tmp/auto-tab-groups-shot.txt"


def main():
    target = sys.argv[1]
    with open(HEIGHT_FILE, encoding="utf-8") as handle:
        height = int(handle.read().strip())
    image = Image.open(target)
    image.crop((0, 0, image.width, height)).save(target)
    print(f"cut {target} to {image.width}x{height}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

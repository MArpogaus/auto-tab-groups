#!/usr/bin/env python3
"""Cut the tab bars out of the frames that shots.el exported.

Both pictures get the same size, so the README shows them one under
the other without a jump.

Usage: python3 demo/shots.py img
"""
import os
import sys

from PIL import Image

HEIGHT_FILE = "/tmp/auto-tab-groups-shot.txt"


def main():
    directory = sys.argv[1]
    with open(HEIGHT_FILE, encoding="utf-8") as handle:
        bars = [line.split() for line in handle if line.strip()]
    height = max(int(bar[1]) for bar in bars)
    for name, _ in bars:
        path = os.path.join(directory, name)
        image = Image.open(path)
        image.crop((0, 0, image.width, height)).save(path)
        print(f"cut {path} to {image.width}x{height}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

[![Build Status](https://travis-ci.org/fourier/maze-gen.svg?branch=master)](https://travis-ci.org/fourier/maze-gen)
# Maze Gen - Procedural maze generation in Common Lisp
## Features
Currently implemented the following algorithms:
- Binary tree
- Sidewinder
- Aldous-Broder
- Wilson's
- Hunt-and-Kill
- Recursive Backtracker"

It is possible to automatically generate the longest path. The distance
between start/end points of the longest path could be graphically presented
as an intensity of the color.

It is possible to export generated maze as either a png image, or to the
format which is accepted by the TrenchBroom editor of maps for Quake1, 
effectively to preview the maze in 3d.

## Examples/Screenshots

![example1](https://github.com/fourier/maze-gen/raw/screenshots/screenshot1.png "Example 1")
![example2](https://github.com/fourier/maze-gen/raw/screenshots/screenshot2.png "Example 2")

```lisp
MAZE-GEN 74 > (grid-draw (binary-tree (make-instance 'grid :rows 20 :cols 20)))
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|                                                                               |
+   +---+   +   +---+---+   +   +   +---+---+   +---+---+   +---+   +   +   +   +
|   |       |   |           |   |   |           |           |       |   |   |   |
+---+---+---+---+   +   +   +---+---+---+   +---+---+   +   +   +---+---+   +   +
|                   |   |   |               |           |   |   |           |   |
+   +---+   +   +---+---+   +   +   +   +   +---+   +   +---+   +   +   +---+   +
|   |       |   |           |   |   |   |   |       |   |       |   |   |       |
+---+   +   +---+   +   +---+   +---+---+   +   +   +---+   +---+---+   +   +   +
|       |   |       |   |       |           |   |   |       |           |   |   |
+   +   +---+   +   +---+   +   +   +---+---+   +---+   +   +---+---+   +   +   +
|   |   |       |   |       |   |   |           |       |   |           |   |   |
+   +   +   +   +   +   +   +   +   +   +   +   +   +---+---+---+   +---+---+   +
|   |   |   |   |   |   |   |   |   |   |   |   |   |               |           |
+---+   +---+---+   +   +   +---+   +---+---+   +   +   +   +   +   +---+---+   +
|       |           |   |   |       |           |   |   |   |   |   |           |
+---+---+---+   +---+---+   +   +   +---+   +   +---+---+   +   +   +   +   +   +
|               |           |   |   |       |   |           |   |   |   |   |   |
+---+---+---+---+---+---+   +   +   +---+   +---+   +---+---+---+   +---+   +   +
|                           |   |   |       |       |               |       |   |
+   +---+   +   +   +---+---+---+---+   +   +---+   +---+   +---+   +---+   +   +
|   |       |   |   |                   |   |       |       |       |       |   |
+---+   +   +---+---+   +---+---+---+---+---+---+   +   +   +---+   +---+   +   +
|       |   |           |                           |   |   |       |       |   |
+---+   +---+   +   +---+---+   +---+   +---+   +---+---+---+---+   +   +---+   +
|       |       |   |           |       |       |                   |   |       |
+   +---+   +---+   +   +   +---+---+   +   +   +   +   +---+---+---+   +   +   +
|   |       |       |   |   |           |   |   |   |   |               |   |   |
+---+   +   +---+   +---+---+   +   +---+---+   +   +---+---+   +---+---+---+   +
|       |   |       |           |   |           |   |           |               |
+---+   +---+---+---+   +   +---+   +   +   +   +   +   +   +---+---+   +   +   +
|       |               |   |       |   |   |   |   |   |   |           |   |   |
+---+   +---+---+   +---+---+---+---+---+   +   +   +   +---+   +   +---+---+   +
|       |           |                       |   |   |   |       |   |           |
+---+   +---+   +---+---+---+---+   +---+   +   +   +   +---+   +---+   +---+   +
|       |       |                   |       |   |   |   |       |       |       |
+---+   +   +---+   +---+   +---+---+   +   +   +---+---+---+   +---+   +   +   +
|       |   |       |       |           |   |   |               |       |   |   |
+---+   +---+   +   +   +---+   +   +   +---+   +---+   +---+---+   +---+---+   +
|       |       |   |   |       |   |   |       |       |           |           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#<grid 236B07D3>
```

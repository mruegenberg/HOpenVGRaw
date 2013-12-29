This directory contains mostly a minimal usage example of ShivaVG. It requires [GLFW](http://www.glfw.org/) to provide the OpenGL context that ShivaVG uses.

## Compiling the C tests
Assuming ShivaVG is in `/usr/local/{include,lib}`, the following should work on most Unix-like systems:
`gcc test.c -I/usr/local/include -L/usr/local/lib -lOpenVG -lglfw3`
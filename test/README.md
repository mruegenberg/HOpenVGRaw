This directory contains mostly a minimal usage example of ShivaVG.

## Compiling the C tests
Assuming ShivaVG is in `/usr/local/{include,lib}`:
`gcc test.c -I/usr/local/include -L/usr/local/lib -lOpenVG -lglfw3 -framework OpenGL`

If you do not use OS X, a line similar to 
`gcc test.c -I/usr/local/include -L/usr/local/lib -lOpenVG -lglfw3 -lGL` (possibly use *-lOpenGL* instead of *-lGL*) should work.
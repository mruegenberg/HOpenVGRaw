#include <vg/openvg.h>
#include <stdlib.h>
#include <stdio.h>
#include <GLFW/glfw3.h>
#include <math.h>

#define WIDTH 500
#define HEIGHT 300

// properties of the square: translation, scale, rotation
VGfloat tx=0.0f, ty=0.0f; // translation
VGfloat sx=1.0f, sy=1.0f; // scale
VGfloat a=0.0f; // rotation

VGfloat sqx = 200; // size of the square
VGfloat sqy = 200;

// square and background color
VGPaint redFill;
VGPath p;

VGfloat red[] = {1,0,0,1};

void cleanup() {
    vgDestroyContextSH();
}

void testMoveTo(VGPath p, float x, float y, VGPathAbsRel absrel) {
  VGubyte seg = VG_MOVE_TO | absrel;
  VGfloat data[2];
  
  data[0] = x; data[1] = y;
  vgAppendPathData(p, 1, &seg, data);
}

void testLineTo(VGPath p, float x, float y, VGPathAbsRel absrel) {
  VGubyte seg = VG_LINE_TO | absrel;
  VGfloat data[2];
  
  data[0] = x; data[1] = y;
  vgAppendPathData(p, 1, &seg, data);
}

void testClosePath(VGPath p) {
  VGubyte seg = VG_CLOSE_PATH;
  VGfloat data = 0.0f;
  vgAppendPathData(p, 1, &seg, &data);
}

VGPath testCreatePath() {
  return vgCreatePath(VG_PATH_FORMAT_STANDARD, VG_PATH_DATATYPE_F,
                      1,0,0,0, VG_PATH_CAPABILITY_ALL);
}

void init() {
    p = testCreatePath();  
    redFill   = vgCreatePaint();
    vgSetParameterfv(redFill, VG_PAINT_COLOR, 4, red);
  
    {
        testMoveTo(p, (WIDTH-sqx)/2, (HEIGHT-sqy)/2, VG_ABSOLUTE);
        testLineTo(p, sqx, 0, VG_RELATIVE);
        testLineTo(p, 0, sqy, VG_RELATIVE);
        testLineTo(p, -sqx, 0, VG_RELATIVE);
        testClosePath(p);
    }
}

void display() {
    VGfloat cc[] = {0,0,0,1};
  
    vgSetfv(VG_CLEAR_COLOR, 4, cc);
    vgClear(0,0,WIDTH,HEIGHT);
  
    vgSeti(VG_MATRIX_MODE, VG_MATRIX_FILL_PAINT_TO_USER);
    vgLoadIdentity();
    vgTranslate(tx, tx);
    vgScale(sx, sy);
    vgRotate(a);
  
    vgSeti(VG_MATRIX_MODE, VG_MATRIX_PATH_USER_TO_SURFACE);
    vgLoadIdentity();
  
    vgSetPaint(redFill, VG_FILL_PATH);
    vgDrawPath(p, VG_FILL_PATH);
}

int main(int argc, char **argv) {
    printf("Hello, World!\n");
    
    atexit(cleanup);
    
    // ShivaVG needs and existing OpenGL context, so we create one here
    int w = WIDTH;
    int h = HEIGHT;
    
    GLFWwindow* window;

    // Initialize the library
    if (!glfwInit()) return -1;
    
    // Create a windowed mode window and its OpenGL context
    window = glfwCreateWindow(w, h, "Hello World", NULL, NULL);
    if (!window) { glfwTerminate(); return -1; }

    // Make the window's context current
    glfwMakeContextCurrent(window);
    
    vgCreateContextSH(w,h);
    
    init();

    // Loop until the user closes the window
    while (!glfwWindowShouldClose(window)) {
        display();

        // Swap front and back buffers
        glfwSwapBuffers(window);

        // Poll for and process events
        glfwPollEvents();
    }
}

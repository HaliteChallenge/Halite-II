#ifndef LUTIL_H
#define LUTIL_H

#include "OpenGL.h"
#include <time.h>

///Screen constants
const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 800;

void initShaderHandler(bool uniqueness);

bool shaderFromFile(GLuint shader, std::string filename, std::string shadername);

#endif

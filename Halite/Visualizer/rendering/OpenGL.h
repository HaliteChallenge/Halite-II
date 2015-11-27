#pragma once

#define GLEW_STATIC

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <string>
#include <vector>
#include <time.h>

///Screen constants
const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 800;

//Color struct.
struct Color
{
	float r, g, b;
};

namespace util
{
	void initShaderHandler(std::fstream * ds);

	bool shaderFromFile(GLuint shader, std::string filename, std::string shadername);
}
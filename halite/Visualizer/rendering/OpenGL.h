#pragma once

#define GLEW_STATIC

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <time.h>
#include "ft2build.h"
#include FT_FREETYPE_H

//Color struct.
struct Color
{
	float r, g, b;
};
static bool operator==(const Color & c1, const Color & c2)
{
	return c1.r == c2.r && c1.g == c2.g && c1.b == c2.b;
}

namespace util
{
	void initShaderHandler(std::fstream * ds);
	//Returns true if all went well; false otherwise (or throws an exception).
	bool shaderFromFile(GLuint shader, std::string filename, std::string shadername);
	void setScreenSize(int w, int h);
	bool initText();
	bool setFont(std::string path);
	//Returns 0 if all went well; the character it failed on otherwise.
	char renderText(float x, float y, int size, Color c, const std::string & text);
	void addText(float x, float y, int size, Color c, const std::string & text);
	char renderAllText(GLFWwindow * w);
	void cleanup();
}
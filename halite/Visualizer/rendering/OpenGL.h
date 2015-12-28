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
#include <time.h>
#include "ft2build.h"
#include FT_FREETYPE_H

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
	//Returns true if all went well; false otherwise (or throws an exception).
	bool shaderFromFile(GLuint shader, std::string filename, std::string shadername);
	void setScreenSize(int w, int h);
	bool initText();
	bool setFont(std::string path);
	//Returns 0 if all went well; the character it failed on otherwise.
	char renderText(float x, float y, int size, const std::string & text);
	void addText(float x, float y, int size, const std::string & text);
	char renderAllText();
	void cleanup();
}
#pragma once

#define GLEW_STATIC

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <string>
#include <vector>
#include <set>
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
	void addFontSize(int s); //If font size is already present, returns immediately (little cost).
	void removeAllFontSizes(); //If font size is not already present, returns immediately (little cost).
	bool setFont(std::string path); //Now going to create text atlas here as well.
	//Returns 0 if all went well; the character it failed on otherwise.
	char renderText(float x, float y, int size, Color c, const std::string & text);
	void cleanup();
}
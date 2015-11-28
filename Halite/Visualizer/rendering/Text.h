#pragma once

//Based on the Tutorial found at http://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_01.

#include <ft2build.h>
#include FT_FREETYPE_H
/*
#include <freetype/freetype.h>
#include <freetype/ftglyph.h>
#include <freetype/ftoutln.h>
#include <freetype/fttrigon.h>
*/

#include <stdexcept>
#include <fstream>

#include "OpenGL.h"

class Text
{
private:
	FT_Face face;
	GLuint tex, buffer, attributes;
public:
	static void init(std::fstream * debug); //Call init before creating any Text classes. Returns success.
	static void cleanup(); //Call when done with all text rendering. Inverse equivalent of init.
	Text(std::string font, int size);
	void resize(int size);
	void render(std::string text, float x, float y, float sx, float sy, Color c);
	~Text();
};
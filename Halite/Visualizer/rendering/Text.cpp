#include "Text.h"

FT_Library ft;
std::fstream * debugfile;
GLuint shaderprogram;

void printDebug(std::string o) { if(debugfile != NULL) *debugfile << o; }

inline int nextP2(int a) { int answer = 1; while(answer < a) answer <<= 1; return answer; }

void Text::init(std::fstream * debug)
{
	debugfile = debug;
	if(FT_Init_FreeType(&ft))
	{
		printDebug("Could not initialize the freetype library.\n");
		throw std::runtime_error("FT_Init_FreeType failed");
	}

	//Create program for rendering.
	GLuint vertexshader, fragmentshader;
	vertexshader = glCreateShader(GL_VERTEX_SHADER);
	if(!util::shaderFromFile(vertexshader, "shaders/text/vertex.glsl", "Text Vertex Shader")) throw std::runtime_error("Vertex Shader sourcing and compilation failed");
	fragmentshader = glCreateShader(GL_FRAGMENT_SHADER);
	if(!util::shaderFromFile(fragmentshader, "shaders/text/fragment.glsl", "Text Fragment Shader")) throw std::runtime_error("Fragment Shader sourcing and compilation failed");

	shaderprogram = glCreateProgram();
	glAttachShader(shaderprogram, vertexshader);
	glAttachShader(shaderprogram, fragmentshader);
	glLinkProgram(shaderprogram);
	glDetachShader(shaderprogram, vertexshader);
	glDetachShader(shaderprogram, fragmentshader);

	glDeleteShader(vertexshader);
	glDeleteShader(fragmentshader);

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void Text::cleanup()
{
	glDeleteProgram(shaderprogram);
}

Text::Text(std::string font, int size)
{
	if(FT_New_Face(ft, font.c_str(), 0, &face))
	{
		printDebug("Could not open font " + font + ".\n");
		throw std::runtime_error("FT_New_Face failed");
	}
	FT_Set_Pixel_Sizes(face, 0, size);

	glActiveTexture(GL_TEXTURE0);
	glGenTextures(1, &tex);
	glBindTexture(GL_TEXTURE_2D, tex);
	glUniform1i(glGetUniformLocation(shaderprogram, "tex"), 0);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glGenBuffers(1, &buffer);
	glBindBuffer(GL_ARRAY_BUFFER, buffer);
	glBindVertexArray(attributes);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
}

void Text::resize(int size)
{
	FT_Set_Pixel_Sizes(face, 0, size);
}

void Text::render(std::string text, float x, float y, float sx, float sy, Color c)
{
	for(auto a = text.begin(); a != text.end(); a++)
	{
		if(FT_Load_Char(face, *a, FT_LOAD_RENDER)) continue; //Skip characters that can't be rendered.

		//Set texture:
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, face->glyph->bitmap.width, face->glyph->bitmap.rows, 0, GL_RED, GL_UNSIGNED_BYTE, face->glyph->bitmap.buffer);

		float x2 = x + face->glyph->bitmap_left * sx;
		float y2 = -y - face->glyph->bitmap_top * sy;
		float w = face->glyph->bitmap.width * sx;
		float h = face->glyph->bitmap.rows * sy;

		GLfloat box[4][4] = {
			{ x2, -y2, 0, 0 },
			{ x2 + w, -y2, 1, 0 },
			{ x2, -y2 - h, 0, 1 },
			{ x2 + w, -y2 - h, 1, 1 },
		};

		glBindBuffer(GL_ARRAY_BUFFER, buffer);
		glBufferData(GL_ARRAY_BUFFER, sizeof(box), box, GL_DYNAMIC_DRAW);
		glUseProgram(shaderprogram);
		float color[] = { c.r, c.g, c.b, 1.0 };
		glUniform4fv(glGetUniformLocation(shaderprogram, "color"), 1, color);
		glBindVertexArray(attributes);
		glEnableVertexAttribArray(0);
		glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

		x += (face->glyph->advance.x >> 6) * sx;
		y += (face->glyph->advance.y >> 6) * sy;
	}
}

Text::~Text()
{
	glDeleteTextures(1, &tex);
	glDeleteBuffers(1, &buffer);
	glDeleteVertexArrays(1, &attributes);
}
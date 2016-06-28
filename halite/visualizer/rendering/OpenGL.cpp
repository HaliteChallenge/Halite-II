#include "OpenGL.hpp"

std::fstream * debugstream;

void util::initShaderHandler(std::fstream * ds)
{
	debugstream = ds;
}

bool util::shaderFromFile(GLuint shader, std::string filename, std::string shadername)
{
	std::fstream in;
	in.open(filename, std::ios_base::in);
	if(!in.is_open())
	{
		*debugstream << "File " << filename << " could not be opened, and consequently <<" << shadername << ">> couldn't be compiled." << std::endl;
		return false;
	}
	in.seekg(0, std::ios::end);
	int length = in.tellg();
	in.seekg(0, std::ios::beg);
	char * file = new GLchar[length];
	in.read(file, length);
	in.close();
	glShaderSource(shader, 1, (const char **)&file, &length);
	glCompileShader(shader);
	delete[] file;
	//Check for proper compilation:
	GLint _compiled = 0;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &_compiled);
	if(_compiled == GL_FALSE)
	{
		GLint length;
		GLchar* log;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
		log = new GLchar[length];
		glGetShaderInfoLog(shader, length, &length, log);
		*debugstream << "There was a problem with shader <<" << shadername << ">>\n" << log << "----------------------------------------" << std::endl;
		delete log;
		return false;
	}
	return true;
}

struct CharTrait
{
	float advance_x, advance_y;
	short bitmap_width, bitmap_rows;
	float bitmap_left, bitmap_top;
	float texture_width, texture_height;
	float texture_x;
};

std::set<int> fontSizes;
std::map< int, std::pair<GLuint, CharTrait * > > atlases;
std::string currentFont = "";

GLuint shaderProgram;
FT_Library ft;
FT_Face face;
int screenWidth, screenHeight;

void util::setScreenSize(int w, int h)
{
	screenWidth = w;
	screenHeight = h;
}

bool util::initText()
{
	shaderProgram = glCreateProgram();
	GLuint vs = glCreateShader(GL_VERTEX_SHADER);
	util::shaderFromFile(vs, "shaders/text/vertex.glsl", "Text Vertex Shader");
	GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
	util::shaderFromFile(fs, "shaders/text/fragment.glsl", "Text Fragment Shader");
	glAttachShader(shaderProgram, fs);
	glAttachShader(shaderProgram, vs);
	glLinkProgram(shaderProgram);
	glDetachShader(shaderProgram, fs);
	glDetachShader(shaderProgram, vs);
	glDeleteShader(vs);
	glDeleteShader(fs);

	glUniform1i(glGetUniformLocation(shaderProgram, "textureSampler"), 0);

	return FT_Init_FreeType(&ft) == 0;
}

void util::addFontSize(int s)
{
	if(std::count(fontSizes.begin(), fontSizes.end(), s)) return;

	fontSizes.insert(s);

	FT_Set_Pixel_Sizes(face, s, s);

	atlases[s] = { 0, 0 }; //Insert
	atlases[s].second = new CharTrait[128];
	unsigned int texWidth = 0, texHeight = 0;
	FT_GlyphSlot g = face->glyph;
	for(int i = 32; i < 128; i++)
	{
		if(FT_Load_Char(face, i, FT_LOAD_RENDER))
		{
			*debugstream << "Loading character " << char(i) << " failed in font " << currentFont << ".\n";
			continue;
		}

		texWidth += g->bitmap.width;
		if(texHeight < g->bitmap.rows) texHeight = g->bitmap.rows;
	}
	glDeleteTextures(1, &atlases[s].first);
	glGenTextures(1, &atlases[s].first);
	glBindTexture(GL_TEXTURE_2D, atlases[s].first);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glActiveTexture(GL_TEXTURE0);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight, 0, GL_RED, GL_UNSIGNED_BYTE, 0);

	int x = 0;
	for(int i = 32; i < 128; i++)
	{
		if(FT_Load_Char(face, i, FT_LOAD_RENDER)) continue;

		glTexSubImage2D(GL_TEXTURE_2D, 0, x, 0, g->bitmap.width, g->bitmap.rows, GL_RED, GL_UNSIGNED_BYTE, g->bitmap.buffer);

		atlases[s].second[i] = { float(g->advance.x) / 64 / screenWidth, float(g->advance.y) / 64 / screenHeight, short(g->bitmap.width), short(g->bitmap.rows), float(g->bitmap_left), float(g->bitmap_top), (float(g->bitmap.width) / texWidth), (float(g->bitmap.rows) / texHeight), float(x) / texWidth };

		x += g->bitmap.width;
	}
}

void util::removeAllFontSizes()
{
	for(auto a = fontSizes.begin(); a != fontSizes.end(); a++)
	{
		glDeleteTextures(1, &atlases[*a].first);
		delete[] atlases[*a].second;
	}
	fontSizes.clear();
	atlases.clear();
}

bool util::setFont(std::string path)
{
	if(path == currentFont) return true;
	if(FT_New_Face(ft, path.c_str(), 0, &face) != 0) return false;

	currentFont = path;
	for(auto a = fontSizes.begin(); a != fontSizes.end(); a++)
	{
		FT_Set_Pixel_Sizes(face, 0, *a);

		delete[] atlases[*a].second;
		atlases[*a].second = new CharTrait[128];

		unsigned int texWidth = 0, texHeight = 0;
		FT_GlyphSlot g = face->glyph;
		for(int i = 32; i < 128; i++)
		{
			if(FT_Load_Char(face, i, FT_LOAD_RENDER))
			{
				*debugstream << "Loading character " << char(i) << " failed in font " << path << ".\n";
				continue;
			}

			texWidth += g->bitmap.width;
			if(texHeight < g->bitmap.rows) texHeight = g->bitmap.rows;
		}
		glDeleteTextures(1, &atlases[*a].first);
		glGenTextures(1, &atlases[*a].first);
		glBindTexture(GL_TEXTURE_2D, atlases[*a].first);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glActiveTexture(GL_TEXTURE0);
		glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight, 0, GL_RED, GL_UNSIGNED_BYTE, 0);

		int x = 0;
		for(int i = 32; i < 128; i++)
		{
			if(FT_Load_Char(face, i, FT_LOAD_RENDER)) continue;

			glTexSubImage2D(GL_TEXTURE_2D, 0, x, 0, g->bitmap.width, g->bitmap.rows, GL_RED, GL_UNSIGNED_BYTE, g->bitmap.buffer);

			atlases[*a].second[i] = { float(g->advance.x) / 64 / screenWidth, float(g->advance.y) / 64 / screenHeight, short(g->bitmap.width), short(g->bitmap.rows), float(g->bitmap_left), float(g->bitmap_top), (float(g->bitmap.width) / texWidth), (float(g->bitmap.rows) / texHeight), float(x) / texWidth };

			x += g->bitmap.width;
		}
	}

	return true;
}

#include <iostream>

char util::renderText(float x, float y, int size, Color c, const std::string & text)
{
	//Get currently bound program:
	GLint currentProgram;
	glGetIntegerv(GL_CURRENT_PROGRAM, &currentProgram);

	//Create initial buffer:
	float points[] = { 0, 0, 0, 0, 0, 0, 0, 0 }; //Zero-initialize
	GLuint vbo = 0;
	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), points, GL_DYNAMIC_DRAW);

	float tpoints[] = { 0, 0, 0, 0, 0, 0, 0, 0 }; //Zero-initialize
	GLuint vbot = 0;
	glGenBuffers(1, &vbot);
	glBindBuffer(GL_ARRAY_BUFFER, vbot);
	glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), tpoints, GL_DYNAMIC_DRAW);

	GLuint vao = 0;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);
	glBindBuffer(GL_ARRAY_BUFFER, vbot);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	glUseProgram(shaderProgram);
	glUniform3fv(glGetUniformLocation(shaderProgram, "color"), 1, (GLfloat *)&c);
	glBindVertexArray(vao);
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);

	addFontSize(size);
	CharTrait * charTraits = atlases[size].second;
	glBindTexture(GL_TEXTURE_2D, atlases[size].first);
	glActiveTexture(GL_TEXTURE0);

	//Skip blank characters, load the position:
	for(int a = 0; a < text.size(); a++)
	{
		CharTrait t = charTraits[text[a]];
		//if(t.bitmap_width == 0 || t.bitmap_rows == 0) continue;

		float left = x + float(t.bitmap_left) / screenWidth, top = y + float(t.bitmap_top) / screenHeight, right = left + (float(t.bitmap_width) / screenWidth), bottom = top - (float(t.bitmap_rows) / screenHeight);
		points[0] = left; points[1] = top; points[2] = right; points[3] = top; points[4] = left; points[5] = bottom; points[6] = right; points[7] = bottom;
		glBindBuffer(GL_ARRAY_BUFFER, vbo);
		glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), points, GL_DYNAMIC_DRAW);
		tpoints[0] = t.texture_x; tpoints[1] = 0; tpoints[2] = t.texture_x + t.texture_width; tpoints[3] = 0; tpoints[4] = t.texture_x; tpoints[5] = t.texture_height; tpoints[6] = t.texture_x + t.texture_width; tpoints[7] = t.texture_height;
		glBindBuffer(GL_ARRAY_BUFFER, vbot);
		glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), tpoints, GL_DYNAMIC_DRAW);

		glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

		x += t.advance_x;
		y += t.advance_y;
	}

	glDeleteBuffers(1, &vbo);
	glDeleteBuffers(1, &vbot);
	glDeleteVertexArrays(1, &vao);

	//Set program back to previous program:
	glUseProgram(currentProgram);

	return 0;
}

void util::cleanup()
{
	glDeleteProgram(shaderProgram);
	util::removeAllFontSizes();
	FT_Done_FreeType(ft);
}

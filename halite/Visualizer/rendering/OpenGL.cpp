#include "OpenGL.h"

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
	std::string line, answer;
	while(std::getline(in, line)) answer += line + "\n";
	in.close();
	const char * c_str = answer.c_str();
	GLint size = answer.size();
	glShaderSource(shader, 1, &(c_str), &(size));
	glCompileShader(shader);
	//Check for proper compilation:
	GLint _compiled = 0;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &_compiled);
	if(!_compiled)
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

bool util::setFont(std::string path)
{
	FT_Done_Face(face);
	return FT_New_Face(ft, path.c_str(), 0, &face) == 0;
}

char util::renderText(GLFWwindow * w, float x, float y, int size, const std::string & text)
{
	glfwMakeContextCurrent(w);

	//Set size.
	FT_Set_Pixel_Sizes(face, size, size);

	//Create initial buffer:
	float points[] = { 0, 0, 0, 0, 0, 0, 0, 0 }; //Zero-initialize
	GLuint vbo = 0;
	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), points, GL_STATIC_DRAW);

	float tpoints[] = { 0, 0, 1, 0, 0, 1, 1, 1 };
	GLuint vbot = 0;
	glGenBuffers(1, &vbot);
	glBindBuffer(GL_ARRAY_BUFFER, vbot);
	glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), tpoints, GL_STATIC_DRAW);

	GLuint vao = 0;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, NULL);
	glBindBuffer(GL_ARRAY_BUFFER, vbot);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, NULL);

	GLuint tex;
	glGenTextures(1, &tex);
	glBindTexture(GL_TEXTURE_2D, tex);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glActiveTexture(GL_TEXTURE0);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	FT_GlyphSlot g = face->glyph;

	//For each character, load the position:
	for(int a = 0; a < text.size(); a++)
	{
		//std::cout << text[a];

		if(FT_Load_Char(face, text[a], FT_LOAD_RENDER) != 0) return text[a];

		float left = x + float(g->bitmap_left) / screenWidth, top = y + float(g->bitmap_top) / screenHeight, right = left + (float(g->bitmap.width) / screenWidth), bottom = top - (float(g->bitmap.rows) / screenHeight);
		points[0] = left; points[1] = top; points[2] = right; points[3] = top; points[4] = left; points[5] = bottom; points[6] = right; points[7] = bottom;
		glBindBuffer(GL_ARRAY_BUFFER, vbo);
		glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), points, GL_STATIC_DRAW);

		glActiveTexture(GL_TEXTURE0);
		glBindTexture(GL_TEXTURE_2D, tex);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, g->bitmap.width, g->bitmap.rows, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, g->bitmap.buffer);
		
		glUseProgram(shaderProgram);
		glBindVertexArray(vao);
		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

		x += float(g->advance.x) / 64 / screenWidth;
		y += float(g->advance.y) / 64 / screenHeight;
	}

	glDeleteBuffers(1, &vbo);
	glDeleteBuffers(1, &vbot);
	glDeleteVertexArrays(1, &vao);
	glDeleteTextures(1, &tex);

	return 0;
}

void util::cleanup()
{
	glDeleteProgram(shaderProgram);
}
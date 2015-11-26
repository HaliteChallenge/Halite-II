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
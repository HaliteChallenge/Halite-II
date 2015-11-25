#ifndef OPENGL_H
#define OPENGL_H

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

void initShaderHandler(bool uniqueness);

bool shaderFromFile(GLuint shader, std::string filename, std::string shadername);

namespace util
{
	static std::fstream debugstream;
	static std::string debugfilename;

	static void initShaderHandler(bool uniqueness)
	{
		if(uniqueness) debugfilename = "logs/debug_" + std::to_string(time(NULL)) + ".log";
		else debugfilename = "logs/debug.log";
		debugstream.open(debugfilename, std::ios_base::out);
		debugstream.close();
	}

	static bool shaderFromFile(GLuint shader, std::string filename, std::string shadername)
	{
		std::fstream in;
		in.open(filename, std::ios_base::in);
		if(!in.is_open())
		{
			debugstream.open(debugfilename, std::ios_base::app);
			debugstream << "File " << filename << " could not be opened, and consequently shader " << shadername << " couldn't be compiled." << std::endl;
			debugstream.close();
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
			debugstream.open(debugfilename, std::ios_base::app);
			debugstream << "There was a problem with shader " << shadername << ".\n" << log << "----------------------------------------" << std::endl;
			debugstream.close();
			delete log;
			return false;
		}
		return true;
	}
}

#endif
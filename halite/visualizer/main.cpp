//#define CONSOLE_DEBUG

#include <iostream>
#include <thread>
#include <stdlib.h>
#include <stdio.h>
#include "core/Halite.hpp"

#ifdef _WIN32
	#include <Windows.h>
	#include <direct.h>
#else
	#include <unistd.h>
#endif


GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleCursor(GLFWwindow * w, double x, double y);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleDrop(GLFWwindow * w, int count, const char ** paths);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void setWindowed();
void setFullscreen();
void renderLaunch();

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false, shiftPressed = false, newGame = false, isLaunch = true, mousePressed = false, isWindowed = true, wPressed = false, aPressed = false, sPressed = false, dPressed = false, disregardFullscreenAttempts = true;
float maxFps = 8, turnNumber = 0, graphZoom = 1.0, maxZoom, mouseX, mouseY;
short numTurns, xOffset = 0, yOffset = 0;
int windowedWidth, windowedHeight;

std::string filename;
std::fstream debug;

#ifdef _WIN32
#define argc __argc
#define argv __argv
#endif

#ifdef _WIN2
INT WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstnace, PSTR lpCmdLine, INT nCmdShow)
#else
int main(int argc, const char ** argv)
#endif
{
	if(argc == 2)
	{
		std::string loc(argv[0]);
		std::replace(loc.begin(), loc.end(), '\\', '/');
		loc = loc.substr(0, loc.find_last_of('/'));
#ifdef _WIN32
		_chdir(loc.c_str());
#else
		chdir(loc.c_str());
#endif
	}

	//Open debug:
	std::string debugfilename = "logs/debug.log";
	debug.open(debugfilename, std::ios_base::out);
	if(!debug.is_open()) //If file couldn't be opened.
	{
		debug.open("DEBUG.log", std::ios_base::out);
		debug << "I couldn't find the folder \"logs\" and consequently can't create multiple logs. Please create that folder for me in the future.";
		debug.flush();
	}

	//start GL context and O/S window using the GLFW helper library
	if(!glfwInit())
	{
		debug << "Could not start GLFW3\n";
		return EXIT_FAILURE;
	}

	util::initShaderHandler(&debug);

	window = NULL;
	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	windowedWidth = mode->width * 3 / 4;
	windowedHeight = mode->height * 2 / 3;
	setWindowed();

	const char * glVersion = (const char * )glGetString(GL_VERSION);
	debug << glVersion;
	debug.flush();

	my_game = new Halite();

	if(argc == 2)
	{
		handleDrop(window, 1, (const char **)(argv + 1));
	}
	else
	{
		while(isLaunch && !glfwWindowShouldClose(window)) renderLaunch();
	}

	clock_t c = clock();

	disregardFullscreenAttempts = false;
	while(!glfwWindowShouldClose(window))
	{
		//Limit render rate:
		float delta = float(clock() - c) / CLOCKS_PER_SEC;
#ifdef CONSOLE_DEBUG
		std::cout << "Frame time of " << delta << ".\n";
#endif
		c = clock();

		short turnNumberS = turnNumber;
		my_game->render(window, turnNumberS, graphZoom, mouseX, mouseY, mousePressed, xOffset, yOffset);
		if(abs(turnNumber - float(turnNumberS) >= 1)) turnNumber = turnNumberS; //Means it's gone past the right edge

		//Poll events
		glfwPollEvents();

		if(upPressed && maxFps <= 120) maxFps += maxFps * delta;
		else if(downPressed && maxFps != 4) maxFps -= maxFps * delta;

		if(leftPressed)
		{
			if(shiftPressed) turnNumber -= 5 * maxFps * delta;
			else turnNumber -= maxFps * delta;
		}
		else if(rightPressed)
		{
			if(shiftPressed) turnNumber += 5 * maxFps * delta;
			else turnNumber += maxFps * delta;
		}
		else if(!isPaused) turnNumber += maxFps * delta;
		if(turnNumber < 0) turnNumber = 0;

		if(wPressed) yOffset--;
		if(aPressed) xOffset++;
		if(sPressed) yOffset++;
		if(dPressed) xOffset--;
	}

	return EXIT_SUCCESS;
}

void setWindowed()
{
	GLFWwindow * w = glfwCreateWindow(windowedWidth, windowedHeight, "Halite", NULL, window);
	glfwGetWindowSize(w, &windowedWidth, &windowedHeight);
	if(window != NULL) glfwDestroyWindow(window);
	window = w;
	//Set leopard handler.
	glfwSetKeyCallback(window, handleKeys);
	//Set character handler.
	glfwSetCharCallback(window, handleChars);
	//Set mouse handler
	glfwSetMouseButtonCallback(window, handleMouse);
	//Set cursor handler.
	glfwSetCursorPosCallback(window, handleCursor);
	//Set error callback handler
	glfwSetErrorCallback(handleErrors);
	//Set file drop function
	glfwSetDropCallback(window, handleDrop);
	//Set window resize handler
	glfwSetWindowSizeCallback(window, handleResize);
	//Make context current:
	glfwMakeContextCurrent(window);
	//It's now windowed.
	isWindowed = true;
	//If necessary, initialize GLEW
	if(my_game == NULL)
	{
		glewExperimental = GL_TRUE;
		if(glewInit() != GLEW_OK) exit(EXIT_FAILURE);
	}
	//If possible, fix the Halite's VAOs.
	if(my_game != NULL) my_game->recreateGL();
	//Fix text.
	if(my_game == NULL) //util::cleanup();
	{
		util::initText();
		util::setFont("fonts/FreeSans.ttf"); //Confirmed to be working
	}
	util::setScreenSize(windowedWidth, windowedHeight);
}

void setFullscreen()
{
	if(window != NULL) glfwGetWindowSize(window, &windowedWidth, &windowedHeight);
	GLFWmonitor * primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	GLFWwindow * w = glfwCreateWindow(mode->width, mode->height, "Halite", primary, window);
	if(window != NULL) glfwDestroyWindow(window);
	window = w;
	//Set leopard handler.
	glfwSetKeyCallback(window, handleKeys);
	//Set character handler.
	glfwSetCharCallback(window, handleChars);
	//Set mouse handler
	glfwSetMouseButtonCallback(window, handleMouse);
	//Set cursor handler.
	glfwSetCursorPosCallback(window, handleCursor);
	//Set error callback handler
	glfwSetErrorCallback(handleErrors);
	//Set file drop function
	glfwSetDropCallback(window, handleDrop);
	//Set window resize handler
	glfwSetWindowSizeCallback(window, handleResize);
	//Make context current:
	glfwMakeContextCurrent(window);
	//If possible, fix the Halite's VAOs.
	if(my_game != NULL) my_game->recreateGL();
	//It's now fullscreen.
	isWindowed = false;
	//Fix text.
	util::setScreenSize(mode->width, mode->height);
	//Viewport.
	glViewport(0, 0, mode->width, mode->height);
}

void handleMouse(GLFWwindow * w, int button, int action, int mods)
{
	if(button == GLFW_MOUSE_BUTTON_1)
	{
		mousePressed = action == GLFW_PRESS;
	}
}

void handleCursor(GLFWwindow * w, double x, double y)
{
	int sx, sy; glfwGetWindowSize(window, &sx, &sy);
	mouseX = (float(2 * x) / sx) - 1.0;
	mouseY = (float(-2 * y) / sy) + 1.0;
}

void handleKeys(GLFWwindow * w, int key, int scancode, int action, int mods)
{
	if(key == GLFW_KEY_LEFT && action == GLFW_PRESS)
	{
		leftPressed = true;
		isPaused = true;
	}
	else if(key == GLFW_KEY_RIGHT && action == GLFW_PRESS)
	{
		rightPressed = true;
		isPaused = true;
	}
	else if(key == GLFW_KEY_LEFT && action == GLFW_RELEASE)
	{
		leftPressed = false;
	}
	else if(key == GLFW_KEY_RIGHT && action == GLFW_RELEASE)
	{
		rightPressed = false;
	}
	else if(key == GLFW_KEY_UP && action == GLFW_PRESS)
	{
		upPressed = true;
	}
	else if(key == GLFW_KEY_DOWN && action == GLFW_PRESS)
	{
		downPressed = true;
	}
	else if(key == GLFW_KEY_UP && action == GLFW_RELEASE)
	{
		upPressed = false;
	}
	else if(key == GLFW_KEY_DOWN && action == GLFW_RELEASE)
	{
		downPressed = false;
	}
	else if((key == GLFW_KEY_LEFT_SHIFT || key == GLFW_KEY_RIGHT_SHIFT) && action == GLFW_PRESS)
	{
		shiftPressed = true;
	}
	else if((key == GLFW_KEY_LEFT_SHIFT || key == GLFW_KEY_RIGHT_SHIFT) && action == GLFW_RELEASE)
	{
		shiftPressed = false;
	}
	else if(key == GLFW_KEY_W && action == GLFW_PRESS)
	{
		wPressed = true;
	}
	else if(key == GLFW_KEY_W && action == GLFW_RELEASE)
	{
		wPressed = false;
	}
	else if(key == GLFW_KEY_S && action == GLFW_PRESS)
	{
		sPressed = true;
	}
	else if(key == GLFW_KEY_S && action == GLFW_RELEASE)
	{
		sPressed = false;
	}
	else if(key == GLFW_KEY_A && action == GLFW_PRESS)
	{
		aPressed = true;
	}
	else if(key == GLFW_KEY_A && action == GLFW_RELEASE)
	{
		aPressed = false;
	}
	else if(key == GLFW_KEY_D && action == GLFW_PRESS)
	{
		dPressed = true;
	}
	else if(key == GLFW_KEY_D && action == GLFW_RELEASE)
	{
		dPressed = false;
	}
	else if(key == GLFW_KEY_ESCAPE)
	{
		exit(0);
	}
}

void handleChars(GLFWwindow * w, unsigned int code)
{
	if(code == ' ') isPaused = !isPaused;
	else if(code == '+')
	{
		graphZoom *= 1.5;
		if(graphZoom > maxZoom) graphZoom = maxZoom;
	}
	else if(code == '-')
	{
		graphZoom /= 1.5;
		if(graphZoom < 1) graphZoom = 1;
	}
	else if(code == '>' || code == '.')
	{
		turnNumber++;
		isPaused = true;
	}
	else if(code == '<' || code == ',')
	{
		turnNumber--;
		isPaused = true;
	}
	else if(code == 'Z' || code == 'z')
	{
		turnNumber = 0;
	}
	else if(code == 'X' || code == 'x')
	{
		turnNumber = numTurns - 1;
	}
	else if(code == 'R' || code == 'r')
	{
		if(filename != "")
		{
			const char * fn = filename.c_str();
			handleDrop(window, 1, &fn);
		}
	}
	else if(code == 'F' || code == 'f')
	{
		if(!disregardFullscreenAttempts) isWindowed ? setFullscreen() : setWindowed();
	}
	else if(code == 'O' || code == 'o')
	{
		xOffset = 0;
		yOffset = 0;
	}
}

void handleDrop(GLFWwindow * w, int count, const char ** paths)
{
	unsigned short wi, he;
	if(my_game->isValid(paths[0]))
	{
		delete my_game;
		my_game = new Halite();
		numTurns = my_game->input(w, paths[0], wi, he);
		filename = paths[0];
		xOffset = 0;
		yOffset = 0;
	}
	else return;
	const int MIN_POINTS_VISIBLE = 3;
	//Set new max_zoom. We allow zooming until only MIN_POINTS_VISIBLE points are visible.
	maxZoom = numTurns / float(MIN_POINTS_VISIBLE);
	isLaunch = false;
	isPaused = false;
	turnNumber = 0;
}

void handleErrors(int error, const char * description)
{
	debug << description;
}

void handleResize(GLFWwindow * w, int width, int height)
{
	glViewport(0, 0, width, height);
	util::setScreenSize(width, height);
	windowedWidth = width;
	windowedHeight = height;
	util::removeAllFontSizes();
}

void renderLaunch()
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	int height; glfwGetWindowSize(window, NULL, &height);
	util::renderText(-.85, 0.0, height / 8, { 1, 1, 1 }, "Drop a replay on-screen to watch it!");

	glfwSwapBuffers(window);
	glfwPollEvents();
}

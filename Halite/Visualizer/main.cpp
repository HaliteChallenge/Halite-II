//#define CONSOLE_DEBUG

#include <iostream>
#include <thread>
#include <Windows.h>
#include "Core/Halite.h"

GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleCursor(GLFWwindow * w, double x, double y);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleDrop(GLFWwindow * w, int count, const char ** paths);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void renderLaunch();

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false, shiftPressed = false, newGame = false, isLaunch = true, mousePressed = false;
float maxFps = 8, turnNumber = 0, graphZoom = 1.0, maxZoom, mouseX, mouseY;
short numTurns;

std::string filename;
std::fstream debug;

FTPixmapFont * t;

#ifdef CONSOLE_DEBUG
int main()
#else
INT WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR lpCmdLine, INT nCmdShow)
#endif
{
	//Open debug:
	std::string debugfilename = "logs/";
	time_t rawtime;
	tm timeinfo;
	time(&rawtime);
	localtime_s(&timeinfo, &rawtime);
	const int STRING_LEN = 30;  char timeC[STRING_LEN]; asctime_s(timeC, STRING_LEN, &timeinfo);
	std::string timeString(timeC);  timeString.pop_back();
	std::replace_if(timeString.begin(), timeString.end(), [](char c) -> bool { return c == ' '; }, '_');
	std::replace_if(timeString.begin(), timeString.end(), [](char c) -> bool { return c == ':'; }, '-');
	debugfilename += timeString.substr(4);
	debugfilename += ".log";
	debug.open(debugfilename, std::ios_base::out);
	if(!debug.is_open()) //If file couldn't be opened.
	{
		debug.open("DEBUG.log", std::ios_base::out);
		debug << "I couldn't find the folder \"logs\" and consequently can't create multiple logs. Please create that folder for me in the future.";
		debug.flush();
	}

	// start GL context and O/S window using the GLFW helper library
	if(!glfwInit())
	{
		debug << "Could not start GLFW3\n";
		return EXIT_FAILURE;
	}

	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);

	window = glfwCreateWindow(mode->width * 2 / 3, mode->height * 2 / 3, "Halite", NULL, NULL);
	if(!window)
	{
		debug << "Could not open window with GLFW3\n";
		glfwTerminate();
		return EXIT_FAILURE;
	}

	glfwMakeContextCurrent(window);

	glewExperimental = GL_TRUE;
	if(glewInit() != GLEW_OK) return EXIT_FAILURE;

	const char * glVersion = (const char * )glGetString(GL_VERSION);
	debug << glVersion;
	debug.flush();

	util::initShaderHandler(&debug);
	t = new FTPixmapFont("fonts/FreeSans.ttf");
	t->FaceSize(48);

	//Set handlers:

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

	my_game = new Halite();
	while(isLaunch && !glfwWindowShouldClose(window)) renderLaunch();

	clock_t c = clock();
	while(!glfwWindowShouldClose(window))
	{
		//Limit render rate:
		float delta = float(clock() - c) / CLOCKS_PER_SEC;
		c = clock();

		short turnNumberS = turnNumber;
		my_game->render(window, turnNumberS, graphZoom, mouseX, mouseY, mousePressed);
		if(abs(turnNumber - float(turnNumberS) > 1)) turnNumber = turnNumberS; //Means it's gone past the right edge

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
	}

	return EXIT_SUCCESS;
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
}

void handleDrop(GLFWwindow * w, int count, const char ** paths)
{
	unsigned short wi, he;
	try
	{
		numTurns = my_game->input(w, paths[0], wi, he);
	}
	catch(std::runtime_error e)
	{
		debug << e.what();
#ifdef CONSOLE_DEBUG
		std::cout << e.what();
#endif
		return;
	}
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
}

void renderLaunch()
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	util::renderText(t, window, -.85, 0.0, 36, "Drop a replay on-screen to watch it!");

	glfwSwapBuffers(window);
	glfwPollEvents();
}
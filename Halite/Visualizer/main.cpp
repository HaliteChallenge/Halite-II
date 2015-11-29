#include <iostream>
#include <thread>
#include <Windows.h>
#include "Core/Halite.h"

#define CONSOLE_DEBUG
//#define USE_TEXT

GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleDrop(GLFWwindow * w, int count, const char ** paths);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void renderLaunch();

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false, shiftPressed = false, newGame = false, isLaunch = true;
signed short turnNumber = 0, maxFps = 8;
float graphZoom = 1.0, maxZoom;

std::string filename;
std::fstream debug;

#ifdef USE_TEXT
Text * t;
#endif

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
	debugfilename += timeString;
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

	util::initShaderHandler(&debug);

#ifdef USE_TEXT
	Text::init(&debug);
	t = new Text("fonts/FreeSans.ttf", 36);
#endif

	//Set handlers:

	//Set leopard handler.
	glfwSetKeyCallback(window, handleKeys);
	//Set character handler.
	glfwSetCharCallback(window, handleChars);
	//Set mouse handler
	glfwSetMouseButtonCallback(window, handleMouse);
	//Set error callback handler
	glfwSetErrorCallback(handleErrors);
	//Set file drop function
	glfwSetDropCallback(window, handleDrop);
	//Set window resize handler
	glfwSetWindowSizeCallback(window, handleResize);

	while(isLaunch && !glfwWindowShouldClose(window)) renderLaunch();

	clock_t c = clock();
	while(!glfwWindowShouldClose(window))
	{
		my_game->render(window, turnNumber, graphZoom);

		if(upPressed && maxFps <= 120) maxFps++;
		else if(downPressed && maxFps != 4) maxFps--;

		if(leftPressed)
		{
			if(shiftPressed) turnNumber -= 15;
			else turnNumber--;
		}
		else if(rightPressed)
		{
			if(shiftPressed) turnNumber += 15;
			else turnNumber++;
		}
		else if(!isPaused) turnNumber++;

		//Limit render rate:
		float delta = 1000.0 * float(clock() - c) / CLOCKS_PER_SEC;
		//std::cout << delta << std::endl;
		if(delta < 1000.0 / maxFps)
		{
			std::this_thread::sleep_for(std::chrono::milliseconds(int(1000.0 / maxFps - delta)));
		}
		c = clock();
	}

	return EXIT_SUCCESS;
}

void handleMouse(GLFWwindow * w, int button, int action, int mods)
{

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
		turnNumber = 1000;
	}
}

void handleDrop(GLFWwindow * w, int count, const char ** paths)
{
	unsigned short wi, he;
	delete my_game;
	my_game = new Halite();
	short numTurns;
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

	//Fill me in with actual rendering later:
#ifdef USE_TEXT
	t->render("Sphinx of black quartz, judge my vow", -0.5, 0.8, 1, 1, { 1.0, 1.0, 0.5 });
#endif

	glfwSwapBuffers(window);
	glfwPollEvents();
}
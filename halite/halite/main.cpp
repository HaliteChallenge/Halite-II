#include <iostream>
#include <cctype>
#include <iostream>
#include <thread>
#include <stdlib.h>
#include <Windows.h>
#include <direct.h>

#include "Core/Halite.h"

GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleCursor(GLFWwindow * w, double x, double y);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void handleLogic();

void setWindowed();
void setFullscreen();
void renderLaunch();
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = true, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false, shiftPressed = false, newGame = false, isLaunch = true, mousePressed = false, isWindowed = true, wPressed = false, aPressed = false, sPressed = false, dPressed = false, disregardFullscreenAttempts = true;
float maxFps = 8, turnNumber = 0, graphZoom = 1.0, mouseX, mouseY;
short xOffset = 0, yOffset = 0;
int windowedWidth, windowedHeight;
std::thread logicThread;
std::fstream debug;

// Returns true if all the arguments required of a user to run a game of Halite are present
// 4 arguments are required width, height, name1, name2 in that order (though more names are welcome)
bool allArgumentsPresent(int __argc, char* __argv[])
{
	auto is_number = [](const std::string& s)
	{
		return !s.empty() && std::find_if(s.begin(),
			s.end(), [](char c) { return !std::isdigit(c); }) == s.end();
	};
	// Remember, the executable name counts as an argument
	if(__argc < 5) return false;

	if(is_number(std::string(__argv[1])) && is_number(std::string(__argv[2]))) return true;

	return false;
}

int main()
{
	srand(time(NULL));

	std::string loc(__argv[0]);
	std::replace(loc.begin(), loc.end(), '\\', '/');
	loc = loc.substr(0, loc.find_last_of('/'));
	_chdir(loc.c_str());

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

	util::initShaderHandler(&debug);

	window = NULL;
	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);
	windowedWidth = mode->width * 3 / 4;
	windowedHeight = mode->height * 2 / 3;
	setWindowed();

	const char * glVersion = (const char *)glGetString(GL_VERSION);
	debug << glVersion;
	debug.flush();

	// Parse command line parameters
	if(allArgumentsPresent(__argc, __argv))
	{
		unsigned short mapWidth = atoi(__argv[1]), mapHeight = atoi(__argv[2]);

		Networking networking;
		for(int a = 3; a < __argc; a++)  networking.startAndConnectBot(std::string(__argv[a]));

		my_game = new Halite(mapWidth, mapHeight, networking);
	}
	// The programs arguments were not passed in the run command.
	// Instead, we will ask the user for them
	else
	{
		std::string in;
		unsigned short mapWidth, mapHeight;

		std::cout << "Please enter the width of the map: ";
		std::getline(std::cin, in);
		while(true)
		{
			try
			{
				mapWidth = std::stoi(in);
				break;
			}
			catch(std::exception e)
			{
				std::cout << "That isn't a valid input. Please enter an integer width of the map: ";
				std::getline(std::cin, in);
			}
		}
		std::cout << "Please enter the height of the map: ";
		std::getline(std::cin, in);
		while(true)
		{
			try
			{
				mapHeight = std::stoi(in);
				break;
			}
			catch(std::exception e)
			{
				std::cout << "That isn't a valid input. Please enter an integer height of the map: ";
				std::getline(std::cin, in);
			}
		}


		my_game = new Halite(mapWidth, mapHeight);
	}

	logicThread = std::thread(handleLogic);

	clock_t c = clock();

	disregardFullscreenAttempts = false;
	while(!glfwWindowShouldClose(window))
	{
		//Limit render rate:
		float delta = float(clock() - c) / CLOCKS_PER_SEC;
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

	glfwDestroyWindow(window);
	logicThread.join();

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
		glfwDestroyWindow(window);
		logicThread.join();
		exit(0);
	}
}

void handleChars(GLFWwindow * w, unsigned int code)
{
	if(code == ' ') isPaused = !isPaused;
	else if(code == '+')
	{
		graphZoom *= 1.5;
		short nTurns = my_game->getNumFrames();
		if(graphZoom > float(nTurns) / 3) graphZoom = float(nTurns) / 3; //Must be able to see at least 3 points at once.
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
		turnNumber = SHRT_MAX;
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

void handleLogic()
{
	std::string filename = "../Replays/Output_" + std::to_string(time(NULL)) + ".hlt";

	std::vector< std::pair<unsigned char, unsigned int> > rankings = my_game->runGame();

	try
	{
		my_game->output(filename);
	}
	catch(std::runtime_error e)
	{
		std::cout << e.what() << std::endl << "Failed to output to file. Opening a file at " << filename.substr(11) << std::endl;
		my_game->output(filename.substr(11));
	}

	std::string victoryOut;
	for(unsigned int a = 0; a < rankings.size(); a++) victoryOut += "In place #" + std::to_string(a + 1) + " is " + my_game->getName(rankings[a].first) + " with a score of " + std::to_string(rankings[a].second) + ".\n";
	std::cout << victoryOut;
}
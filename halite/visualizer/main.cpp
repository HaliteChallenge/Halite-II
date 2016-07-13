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
void renderHelp(int height);

Halite * my_game = NULL; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false;
bool wPressed = false, aPressed = false, sPressed = false, dPressed = false;
bool shiftPressed = false, tabPressed = false, hPressed = false, mousePressed = false;
bool newGame = false, isLaunch = true;
bool isWindowed = true, verboseOutput = false;
float maxFps = 8, turnNumber = 0, graphZoom = 1.0, maxZoom, mouseX, mouseY, xOffset = 0, yOffset = 0;
int windowedWidth, windowedHeight, numTurns;

std::string filename;
std::ofstream debug;

#ifdef _WIN32
	#define argc __argc
	#define argv __argv
#endif

//Define shifts.
#if defined(_WIN32)
	#define SHIFT 1
#elif defined(__APPLE__)
	#define SHIFT 2
#else
	#define SHIFT 0.025
#endif

#ifdef _WIN32
INT WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstnace, PSTR lpCmdLine, INT nCmdShow) {
#else
int main(int argc, const char ** argv) {
#endif

	std::string loc(argv[0]);
	std::replace(loc.begin(), loc.end(), '\\', '/');
	loc = loc.substr(0, loc.find_last_of('/'));
#ifdef _WIN32
	_chdir(loc.c_str());
#else
	if(chdir(loc.c_str())) return EXIT_FAILURE;
#endif

	//Open debug:
	debug.open("logs/debug.log", std::ios_base::out | std::ios_base::binary);
	if(!debug.is_open()) debug.open("debug.log", std::ios_base::out);
	debug.flush();

	//start GL context and O/S window using the GLFW helper library
	if(!glfwInit()) {
		debug << "Could not start GLFW3\n";
		return EXIT_FAILURE;
	}

	util::initShaderHandler(&debug);

	window = NULL;
	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	glfwWindowHint(GLFW_REFRESH_RATE, 60);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwSwapInterval(0);
	windowedWidth = mode->width * 3 / 4;
	windowedHeight = mode->height * 2 / 3;
	setWindowed();

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glClearColor(0, 0, 0, 1);

	if(argc > 1) {
		if(strcmp(argv[1], "-v") == 0) {
			verboseOutput = true;
			if(argc == 3) {
				debug << "About to handle the drop of the provided (arg 3) file!" << std::endl;
				handleDrop(window, 1, (const char **)(argv + 2));
			}
			else {
				while(isLaunch && !glfwWindowShouldClose(window)) {
					debug << "About to enter renderLaunch!" << std::endl;
					renderLaunch();
				}
			}
		}
		else if(argc == 2) handleDrop(window, 1, (const char **)(argv + 1));
	}
	else {
		while(isLaunch && !glfwWindowShouldClose(window)) {
			renderLaunch();
		}
	}

	if(verboseOutput) {
		const char * glVersion = (const char * )glGetString(GL_VERSION);
		debug << glVersion;
		debug.flush();
	}

	glfwSwapInterval(1);

	clock_t c = clock();
	
	if(verboseOutput) debug << "Entering main render loop!" << std::endl;
	while(!glfwWindowShouldClose(window)) {
		if(hPressed) {
			float delta = float(clock() - c) / CLOCKS_PER_SEC;
			if(verboseOutput) debug << "[In help render loop] Frame time of " << delta << ".\n";
			c = clock();

			if(verboseOutput) debug << "About to clear color & depth buffer bits in render help loop" << std::endl;
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

			if(verboseOutput) debug << "Getting window height from glfw in render help loop" << std::endl;
			int height; glfwGetWindowSize(window, NULL, &height);

			if(verboseOutput) debug << "Rendering text in render help loop" << std::endl;
			util::renderText(-.85, 0.65, height / 6, { 1, 1, 1 }, "Halite Visualizer Help!");

			renderHelp(height);

			if(verboseOutput) debug << "Swapping buffers in render help loop" << std::endl;
			glfwSwapBuffers(window);
			if(verboseOutput) debug << "Polling events in render help loop" << std::endl;
			glfwPollEvents();

		}
		else {
			//Limit render rate:
			float delta = float(clock() - c) / CLOCKS_PER_SEC;
			if(verboseOutput) debug << "[In game render loop] Frame time of " << delta << ".\n";
			c = clock();

			short turnNumberS = turnNumber;
			if(verboseOutput) debug << "About to render at turn #" << turnNumberS << std::endl;
			my_game->render(window, turnNumberS, graphZoom, mouseX, mouseY, tabPressed, mousePressed, xOffset, yOffset);
			if(verboseOutput) debug << "Just rendered turn #" << turnNumberS << std::endl;
			if(abs(turnNumber - float(turnNumberS) >= 1)) turnNumber = turnNumberS; //Means it's gone past the right edge

			//Poll events
			glfwPollEvents();
			if(verboseOutput) debug << "Polled events in render game loop!" << std::endl;

			if(upPressed && maxFps <= 120) maxFps += maxFps * delta;
			else if(downPressed && maxFps != 4) maxFps -= maxFps * delta;

			if(leftPressed) {
				if(shiftPressed) turnNumber -= 5 * maxFps * delta;
				else turnNumber -= maxFps * delta;
			}
			else if(rightPressed) {
				if(shiftPressed) turnNumber += 5 * maxFps * delta;
				else turnNumber += maxFps * delta;
			}
			else if(!isPaused && !tabPressed) turnNumber += maxFps * delta;
			if(turnNumber < 0) turnNumber = 0;

			if(wPressed) yOffset -= SHIFT;
			if(aPressed) xOffset += SHIFT;
			if(sPressed) yOffset += SHIFT;
			if(dPressed) xOffset -= SHIFT;

			if(verboseOutput) debug << "Finished iteration of render game loop!!" << std::endl;
		}
	}

	return EXIT_SUCCESS;
}

void setWindowed() {
	if(verboseOutput) debug << "Swapping to windowed mode!" << std::endl;
	GLFWwindow * w = glfwCreateWindow(windowedWidth, windowedHeight, "Halite", NULL, window);
	if(verboseOutput) debug << "Successfully created a new window!" << std::endl;
	glfwGetWindowSize(w, &windowedWidth, &windowedHeight);
	if(window != NULL) glfwDestroyWindow(window);
	if(verboseOutput) debug << "Successfully destroyed the old window!" << std::endl;
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
	if(my_game == NULL) {
		glewExperimental = GL_TRUE;
		if(glewInit() != GLEW_OK) exit(EXIT_FAILURE);
	}
	//If possible, fix the Halite's VAOs.
	if(verboseOutput) debug << "Recreating all of the Halite GL objects" << std::endl;
	if(my_game != NULL) my_game->recreateGL();
	if(verboseOutput) debug << "Just recreated all of the Halite GL objects" << std::endl;
	//Fix text.
	if(my_game == NULL) {
		util::initText();
		util::setFont("fonts/FreeSans.ttf"); //Confirmed to be working
	}
	util::setScreenSize(windowedWidth, windowedHeight);
	if(verboseOutput) debug << "Finished swapping to windowed mode!" << std::endl;
}

void setFullscreen() {
	if(verboseOutput) debug << "Swapping to fullscreen mode!" << std::endl;
	if(window != NULL) glfwGetWindowSize(window, &windowedWidth, &windowedHeight);
	GLFWmonitor * primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	GLFWwindow * w = glfwCreateWindow(mode->width, mode->height, "Halite", primary, window);
	if(verboseOutput) debug << "Successfully created a new window!" << std::endl;
	if(window != NULL) glfwDestroyWindow(window);
	if(verboseOutput) debug << "Successfully destroyed the old window!" << std::endl;
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
	if(verboseOutput) debug << "Recreating all of the Halite GL objects" << std::endl;
	if(my_game != NULL) my_game->recreateGL();
	if(verboseOutput) debug << "Just recreated all of the Halite GL objects" << std::endl;
	//It's now fullscreen.
	isWindowed = false;
	//Fix text.
	util::setScreenSize(mode->width, mode->height);
	//Viewport.
	glViewport(0, 0, mode->width, mode->height);
	if(verboseOutput) debug << "Finished swapping to fullscreen!" << std::endl;
}

void handleMouse(GLFWwindow * w, int button, int action, int mods) {
	if(verboseOutput) debug << "I've detected a mouse action! The key has code " << button << " and action " << action << std::endl;
	if(button == GLFW_MOUSE_BUTTON_1) {
		mousePressed = action == GLFW_PRESS;
	}
}

void handleCursor(GLFWwindow * w, double x, double y) {
	if(verboseOutput) debug << "I've detected a cursor movement! The cursor is at location (" << x << ", " << y << ')' << std::endl;
	int sx, sy; glfwGetWindowSize(window, &sx, &sy);
	mouseX = (float(2 * x) / sx) - 1.0;
	mouseY = (float(-2 * y) / sy) + 1.0;
}

void handleKeys(GLFWwindow * w, int key, int scancode, int action, int mods) {
	if(verboseOutput) debug << "I've detected a key action! The key has code " << key << " and action " << action << std::endl;
	if(key == GLFW_KEY_LEFT && action == GLFW_PRESS) {
		leftPressed = true;
		isPaused = true;
	}
	else if(key == GLFW_KEY_RIGHT && action == GLFW_PRESS) {
		rightPressed = true;
		isPaused = true;
	}
	else if(key == GLFW_KEY_LEFT && action == GLFW_RELEASE) {
		leftPressed = false;
	}
	else if(key == GLFW_KEY_RIGHT && action == GLFW_RELEASE) {
		rightPressed = false;
	}
	else if(key == GLFW_KEY_UP && action == GLFW_PRESS) {
		upPressed = true;
	}
	else if(key == GLFW_KEY_DOWN && action == GLFW_PRESS) {
		downPressed = true;
	}
	else if(key == GLFW_KEY_UP && action == GLFW_RELEASE) {
		upPressed = false;
	}
	else if(key == GLFW_KEY_DOWN && action == GLFW_RELEASE) {
		downPressed = false;
	}
	else if((key == GLFW_KEY_LEFT_SHIFT || key == GLFW_KEY_RIGHT_SHIFT) && action == GLFW_PRESS) {
		shiftPressed = true;
	}
	else if((key == GLFW_KEY_LEFT_SHIFT || key == GLFW_KEY_RIGHT_SHIFT) && action == GLFW_RELEASE) {
		shiftPressed = false;
	}
	else if(key == GLFW_KEY_W && action == GLFW_PRESS) {
		wPressed = true;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_W && action == GLFW_RELEASE) {
		wPressed = false;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_S && action == GLFW_PRESS) {
		sPressed = true;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_S && action == GLFW_RELEASE) {
		sPressed = false;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_A && action == GLFW_PRESS) {
		aPressed = true;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_A && action == GLFW_RELEASE) {
		aPressed = false;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_D && action == GLFW_PRESS) {
		dPressed = true;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_D && action == GLFW_RELEASE) {
		dPressed = false;
		yOffset = int(yOffset);
		xOffset = int(xOffset);
	}
	else if(key == GLFW_KEY_TAB && action == GLFW_PRESS) {
		tabPressed = true;
	}
	else if(key == GLFW_KEY_TAB && action == GLFW_RELEASE) {
		tabPressed = false;
	}
	else if(key == GLFW_KEY_H && action == GLFW_PRESS) {
		hPressed = true;
	}
	else if(key == GLFW_KEY_H && action == GLFW_RELEASE) {
		hPressed = false;
	}
	else if(key == GLFW_KEY_ESCAPE) {
		exit(0);
	}
}

void handleChars(GLFWwindow * w, unsigned int code) {
	if(verboseOutput) debug << "A character has been pressed! Specifically, character #" << code << std::endl;
	if(code == ' ') isPaused = !isPaused;
	else if(code == '+') {
		graphZoom *= 1.5;
		if(graphZoom > maxZoom) graphZoom = maxZoom;
	}
	else if(code == '-') {
		graphZoom /= 1.5;
		if(graphZoom < 1) graphZoom = 1;
	}
	else if(code == '>' || code == '.') {
		shiftPressed ? turnNumber += 5 : turnNumber++;
		isPaused = true;
	}
	else if(code == '<' || code == ',') {
		shiftPressed ? turnNumber -= 5 : turnNumber--;
		isPaused = true;
	}
	else if(code == 'Z' || code == 'z') {
		turnNumber = 0;
	}
	else if(code == 'X' || code == 'x') {
		turnNumber = numTurns - 1;
	}
	else if(code == 'R' || code == 'r') {
		if(filename != "") {
			const char * fn = filename.c_str();
			handleDrop(window, 1, &fn);
		}
	}
	else if(code == 'F' || code == 'f') {
		isWindowed ? setFullscreen() : setWindowed();
	}
	else if(code == 'O' || code == 'o') {
		xOffset = 0;
		yOffset = 0;
	}
}

void handleDrop(GLFWwindow * w, int count, const char ** paths) {
	if(verboseOutput) debug << "Handling a drop!" << std::endl;
	unsigned short wi, he;
    if(my_game == NULL) my_game = new Halite();
	if(my_game->isValid(paths[0])) {
		if(verboseOutput) debug << "The file seems to have a valid header." << std::endl;
		delete my_game;
		my_game = new Halite();
		numTurns = my_game->input(w, paths[0], wi, he);
		if(verboseOutput) debug << "Successfully inputted the file." << std::endl;
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
	if(verboseOutput) debug << "Finished handleDrop." << std::endl;
}

void handleErrors(int error, const char * description) {
	if(verboseOutput) debug << "Handling an error via handleErrors - description is:" << std::endl;
	debug << description << std::endl;
}

void handleResize(GLFWwindow * w, int width, int height) {
	if(verboseOutput) debug << "Handling a screen resize with width " << width << " and height " << height << std::endl;
	glViewport(0, 0, width, height);
	util::setScreenSize(width, height);
	windowedWidth = width;
	windowedHeight = height;
	util::removeAllFontSizes();
	if(verboseOutput) debug << "Cleared font sizes in handleResize" << std::endl;
}

void renderLaunch() {
	if(verboseOutput) debug << "Entered renderLaunch!" << std::endl;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	if(verboseOutput) debug << "renderLaunch - cleared color and depth buffer bits!" << std::endl;

	int height; glfwGetWindowSize(window, NULL, &height);
	util::renderText(-.85, 0.65, height / 6, { 1, 1, 1 }, "Drop a replay on-screen to watch it!");
	renderHelp(height);
	util::renderText(-.85, -0.84, height / 12, { 1, 1, 1 }, " - To view this help panel, hold h");

	if(verboseOutput) debug << "renderLaunch - rendered necessary text!" << std::endl;

	glfwSwapBuffers(window);
	if(verboseOutput) debug << "renderLaunch - swapped buffers!" << std::endl;
	glfwPollEvents();
	if(verboseOutput) debug << "renderLaunch - polled events!" << std::endl;
}

void renderHelp(int height) {
	if(verboseOutput) debug << "Entering renderHelp!" << std::endl;
	util::renderText(-.85, 0.48, height / 12, { 1, 1, 1 }, " - To pause or unpause the replay, press SPACE");
	util::renderText(-.85, 0.36, height / 12, { 1, 1, 1 }, " - To move around in the replay, press LEFT ARROW or RIGHT ARROW");
	util::renderText(-.85, 0.28, height / 16, { 1, 1, 1 }, "     - Hold shift to move around five times faster.");
	util::renderText(-.85, 0.20, height / 16, { 1, 1, 1 }, "     - To change move and play speed, press UP ARROW or DOWN ARROW");
	util::renderText(-.85, 0.08, height / 12, { 1, 1, 1 }, " - To move around in the replay by frame, press , or .");
	util::renderText(-.85, -0.04, height / 12, { 1, 1, 1 }, " - To go to the beginning or end of the replay, press z or x");
	util::renderText(-.85, -0.16, height / 12, { 1, 1, 1 }, " - To pan around in the map, use the w, a, s, and d keys");
	util::renderText(-.85, -0.24, height / 16, { 1, 1, 1 }, "     - To reset to the origin, press o");
	util::renderText(-.85, -0.36, height / 12, { 1, 1, 1 }, " - To zoom in or out on the graphs, press + or -");
	util::renderText(-.85, -0.48, height / 12, { 1, 1, 1 }, " - To view the production map, hold TAB");
	util::renderText(-.85, -0.60, height / 12, { 1, 1, 1 }, " - To reload a replay from file, press r");
	util::renderText(-.85, -0.72, height / 12, { 1, 1, 1 }, " - To toggle fullscreen mode, press f");
	if(verboseOutput) debug << "Leaving renderHelp!" << std::endl;
}
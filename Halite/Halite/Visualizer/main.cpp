#include <iostream>

#include "../Core/Halite.h"
#include "Rendering.h"

GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleDrop(GLFWwindow * w, int count, const char ** paths);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void render();

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, upPressed = false, downPressed = false, shiftPressed = false, newGame = false;
signed short turnNumber = 0, maxFps = 30;
float graphZoom = 1.0;

std::string filename;

int main(int argc, char* args[])
{
	// start GL context and O/S window using the GLFW helper library
	if(!glfwInit())
	{
		fprintf(stderr, "Could not start GLFW3\n");
		return EXIT_FAILURE;
	}

	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode * mode = glfwGetVideoMode(primary);
	glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);

	window = glfwCreateWindow(mode->width * 2 / 3, mode->height * 2 / 3, "Halite", NULL, NULL);
	if(!window)
	{
		fprintf(stderr, "Could not open window with GLFW3\n");
		glfwTerminate();
		return EXIT_FAILURE;
	}

	glfwMakeContextCurrent(window);

	glewExperimental = GL_TRUE;
	if(glewInit() != GLEW_OK) return EXIT_FAILURE;

	util::initShaderHandler(true);

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

	my_game = new Halite();
	std::cout << "Simply drop the file onto the window." << std::endl;
	setup(my_game->game_map.map_width, my_game->game_map.map_height, my_game->number_of_players);

	clock_t c = clock();
	while(!glfwWindowShouldClose(window))
	{
		render();

		if(upPressed && maxFps != 60) maxFps++;
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
			Sleep(1000.0 / maxFps - delta);
		}
		c = clock();
	}

	return 0;
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
		if(graphZoom > 129.7) graphZoom = 129.7463;
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
}

void handleDrop(GLFWwindow * w, int count, const char ** paths)
{
	unsigned short wi, he;
	if(!my_game->input(paths[0], wi, he)) std::cout << "I couldn't open the specified file. Please drop another file onto the window.\n";
	isPaused = false;
	turnNumber = 0;
}

void handleErrors(int error, const char * description)
{
	fprintf(stderr, description);
}

void handleResize(GLFWwindow * w, int width, int height)
{
	glViewport(0, 0, width, height);
}

void render()
{
	//Clear color buffer
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	render(my_game->full_game, turnNumber, graphZoom);

	glfwPollEvents();
	glfwSwapBuffers(window);
}
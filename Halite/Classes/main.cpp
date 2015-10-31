#include <iostream>

#include "Util.h"
#include "GameLogic/Halite.h"

GLFWwindow * window;

void handleMouse(GLFWwindow * w, int button, int action, int mods);
void handleKeys(GLFWwindow * w, int button, int scancode, int action, int mods);
void handleChars(GLFWwindow * w, unsigned int code);
void handleDrop(GLFWwindow * w, int count, const char ** paths);
void handleErrors(int error, const char * description);
void handleResize(GLFWwindow * w, int width, int height);

void render();
void doLogic();

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.
bool isPaused = false, leftPressed = false, rightPressed = false, shiftPressed = false, newGame = false, mapNotGraph = true, territoryNotStrength = false;
signed short turn_number = 0;
float maxFps = 20;

std::string filename;

int main(int argc, char* args[])
{
	std::string in;
	filename = "Replays/Output_" + std::to_string(time(NULL)) + ".hlt";
	std::thread logicThread;
	unsigned short mapWidth, mapHeight;

	std::cout << "Would you like to run a new game or render a past one? Please enter \"New\" or \"Past\": ";
	/*while(true)
	{
		std::getline(std::cin, in);
		std::transform(in.begin(), in.end(), in.begin(), ::tolower);
		if(in == "p" || in == "past" || in == "n" || in == "new") break;
		std::cout << "That isn't a valid input. Please enter \"New\" or \"Past\": ";
	}
	bool newGame = in == "new" || in == "n";*/
	// TEMPORARY: for debugging purposes
	bool newGame = true;

	/*if(newGame)
	{
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
		newGame = true;
	}*/
	// TEMPORARY: Set width and height for debugging purposes
	mapWidth = mapHeight = 10;

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

	initShaderHandler(true);

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

	if(newGame)
	{
		my_game = new Halite(mapWidth, mapHeight);

		my_game->init();

		logicThread = std::thread(doLogic);
	}
	else
	{
		my_game = new Halite();
		std::cout << "Simply drop the file onto the window." << std::endl;
	}

	clock_t c = clock();
	while(!glfwWindowShouldClose(window))
	{
		render();
		if(leftPressed)
		{
			if(shiftPressed) turn_number -= 15;
			else turn_number--;
		}
		else if(rightPressed)
		{
			if(shiftPressed) turn_number += 15;
			else turn_number++;
		}
		else if(!isPaused) turn_number++;

		//Limit render rate:
		float delta = 1000.0 * float(clock() - c) / CLOCKS_PER_SEC;
		//std::cout << delta << std::endl;
		if(delta < 1000.0 / maxFps)
		{
			Sleep(1000.0 / maxFps - delta);
		}
		c = clock();
	}
	

	if(newGame)
	{
		std::cout << "I'm still handling logic.\n";
		logicThread.join();
		std::cout << "I've finished my logic. If you'd like to save to a file, press the 'o' key. Any other key will exit the program.\n";
		char keyPressed = getchar();
		if(keyPressed == 'o' || keyPressed == 'O') my_game->output(filename);
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
		maxFps = maxFps * 1.5 + 1;
		if(maxFps > 100) maxFps = 100;
	}
	else if(code == '-')
	{
		maxFps = maxFps / 1.5 - 1;
		if(maxFps < 1) maxFps = 1;
	}
	else if(code == '>' || code == '.')
	{
		turn_number++;
		isPaused = true;
	}
	else if(code == '<' || code == ',')
	{
		turn_number--;
		isPaused = true;
	}
	else if(code == 'o' || code == 'O' && newGame)
	{
		my_game->output(filename);
	}
	else if(code == 'g' || code == 'G' && newGame)
	{
		mapNotGraph = !mapNotGraph;
	}
	else if(code == 'q' || code == 'Q' && newGame)
	{
		territoryNotStrength = !territoryNotStrength;
	}
}

void handleDrop(GLFWwindow * w, int count, const char ** paths)
{
	unsigned short wi, he;
	if(!my_game->input(paths[0], wi, he)) std::cout << "I couldn't open the specified file. Please drop another file onto the window.\n";
	isPaused = false;
	turn_number = 0;
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

	my_game->render(turn_number);

	glfwPollEvents();
	glfwSwapBuffers(window);
}

void doLogic()
{
	std::vector< std::pair<std::string, float> >rankings = my_game->runGame();
	std::string victoryOut;
	for (unsigned int a = 0; a < rankings.size(); a++) victoryOut += "In ranking " + std::to_string(a + 1) + " is player " + rankings[a].first + " with a relative score of " + std::to_string(rankings[a].second).substr(0, 5) + "\n";
	std::cout << victoryOut;
}
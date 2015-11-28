#include <iostream>

#include "Core/Halite.h"

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

int main(int argc, char* args[])
{
	srand(time(NULL));

	std::string in;
	std::thread logicThread;
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
	std::string filename = std::to_string(time(NULL)) + ".hlt";
	std::cout << "filename: " << filename << "\n";
	my_game = new Halite(mapWidth, mapHeight, filename);
	my_game->init();
	
	std::vector< std::pair<std::string, float> >rankings = my_game->runGame();
	std::string victoryOut;
	for(unsigned int a = 0; a < rankings.size(); a++) victoryOut += "In ranking " + std::to_string(a + 1) + " is player " + rankings[a].first + " with a relative score of " + std::to_string(rankings[a].second).substr(0, 5) + "\n";
	std::cout << victoryOut;

	system("PAUSE");
	return 0;
}
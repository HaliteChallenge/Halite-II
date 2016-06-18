#include <iostream>
#include <cctype>
#include <chrono>

#include "core/Halite.h"

Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

// Returns true if all the arguments required of a user to run a game of Halite are present
// 4 arguments are required width, height, name1, name2 in that order (though more names are welcome)
bool allArgumentsPresent(int argc, char* args[])
{
	auto is_number = [](const std::string& s)
	{
		return !s.empty() && std::find_if(s.begin(),
			s.end(), [](char c) { return !std::isdigit(c); }) == s.end();
	};
	// Remember, the executable name counts as an argument
	if(argc < 5) return false;

	if(is_number(std::string(args[1])) && is_number(std::string(args[2]))) return true;

	return false;
}

int main(int argc, char* args[])
{
	srand(time(NULL));

	// Parse command line parameters
	if(allArgumentsPresent(argc, args))
	{
		unsigned short mapWidth = atoi(args[1]), mapHeight = atoi(args[2]);

		Networking networking;
		for(int a = 3; a < argc; a++)  networking.startAndConnectBot(std::string(args[a]));

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

	std::string filename = "../Replays/" + std::to_string(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock().now().time_since_epoch()).count()) + ".hlt";

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
	for (unsigned int a = 0; a < rankings.size(); a++) victoryOut += "In place #" + std::to_string(a + 1) + " is player "  + std::to_string(rankings[a].first) + " named " + my_game->getName(rankings[a].first) + " with a score of " + std::to_string(rankings[a].second) + "\n";
	std::cout << victoryOut;

    delete my_game;

	system("PAUSE");
	return 0;
}

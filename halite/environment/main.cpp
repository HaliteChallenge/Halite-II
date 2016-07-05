#include <iostream>
#include <cctype>
#include <chrono>
#include <string.h>

#include "core/Halite.hpp"

bool program_output_style;
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

//Returns true if all the arguments required of a user to run a game of Halite are present
//4 arguments are required width, height, name1, name2 in that order (though more names are welcome)
bool allArgumentsPresent(int argc, char* args[]) {
	auto is_number = [](const std::string& s) {
		return !s.empty() && std::find_if(s.begin(),
		s.end(), [](char c) { return !std::isdigit(c); }) == s.end();
	};
	if(argc == 1) return false;
	//Remember, the executable name counts as an argument
	if(strcmp(args[1], "-q") == 0) {
		if(argc < 6) return false;
		if(is_number(std::string(args[2])) && is_number(std::string(args[3]))) return true;
	}
	else {
		if(argc < 5) return false;
		if(is_number(std::string(args[1])) && is_number(std::string(args[2]))) return true;
	}

	return false;
}

int main(int argc, char* args[]) {
	srand(time(NULL));

	//Parse command line parameters
	if(allArgumentsPresent(argc, args)) {
		unsigned short mapWidth, mapHeight;
		Networking networking;

		if(strcmp(args[1], "-q") == 0) {
			program_output_style = true;
			mapWidth = atoi(args[2]);
			mapHeight = atoi(args[3]);
			for(int a = 4; a < argc; a++)  networking.startAndConnectBot(std::string(args[a]));
		}
		else {
			program_output_style = false;
			mapWidth = atoi(args[1]);
			mapHeight = atoi(args[2]);
			for(int a = 3; a < argc; a++)  networking.startAndConnectBot(std::string(args[a]));
		}

		my_game = new Halite(mapWidth, mapHeight, networking);
	}

	//Check if we should give output for the program or give output for the human.

	//The programs arguments were not passed in the run command.
	//Instead, we will ask the user for them
	else{
		program_output_style = false;

		std::string in;
		unsigned short mapWidth, mapHeight;

		std::cout << "Please enter the width of the map: ";
		std::getline(std::cin, in);
		while(true) {
			try{
				mapWidth = std::stoi(in);
				break;
			}
			catch(std::exception e) {
				std::cout << "That isn't a valid input. Please enter an integer width of the map: ";
				std::getline(std::cin, in);
			}
		}
		std::cout << "Please enter the height of the map: ";
		std::getline(std::cin, in);
		while(true) {
			try{
				mapHeight = std::stoi(in);
				break;
			}
			catch(std::exception e) {
				std::cout << "That isn't a valid input. Please enter an integer height of the map: ";
				std::getline(std::cin, in);
			}
		}


		my_game = new Halite(mapWidth, mapHeight);
	}
	std::string filename = "Replays/" + std::to_string(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock().now().time_since_epoch()).count()) + ".hlt";

	GameStatistics stats = my_game->runGame();

	try{
		my_game->output(filename);
	}
	catch(std::runtime_error e) {
		filename = filename.substr(8);
		if(!program_output_style) std::cout << e.what() << std::endl << "Failed to output to file. Opening a file at " << filename << std::endl;
		my_game->output(filename);
	}

	std::string victoryOut;
	if(program_output_style) {
		std::cout << filename << std::endl << stats;
	}
	else {
		for(unsigned int a = 0; a < stats.player_statistics.size(); a++) std::cout << "Player #" << stats.player_statistics[a].tag << ", " << my_game->getName(stats.player_statistics[a].tag) << ", came in rank #" << stats.player_statistics[a].rank << "!\n";
	}

	delete my_game;
	return 0;
}

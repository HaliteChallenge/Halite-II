#include <iostream>
#include <cctype>
#include <chrono>
#include <list>
#include <string.h>

#include "core/Halite.hpp"

bool quiet_output = false, ignore_timeout = false; //Need to be passed to the game.
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

int main(int argc, char* args[]) {
	srand(time(NULL)); //For all non-seeded randomness.

	bool watch_game = false, override_names = false; //Extra parameters. 
	
	//Paramters to start up a game.
	bool passed_dimensions = false, passed_seed = false, passed_bot_names = false;
	unsigned short mapWidth, mapHeight;
	unsigned int seed = (std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() % 4294967295); //Using milliseconds to prevent same maps from coming up due to multiple worker servers.
	Networking networking;
	std::vector<std::string> * names = NULL;

	std::list<std::string> sArgs;
	for(int a = 1; a < argc; a++) sArgs.push_back(args[a]);

	for(auto a = sArgs.begin(); a != sArgs.end();) {
		if(*a == "-d") {
			passed_dimensions = true;
			a = sArgs.erase(a);
			try {
				mapWidth = std::stoll(*a);
				a = sArgs.erase(a);
				mapHeight = std::stoll(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The dimension parameters were either not present or invalid." << std::endl;
				return EXIT_FAILURE;
			}
		}
		if(*a == "-w") {
			watch_game = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-q") {
			quiet_output = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-o") {
			override_names = true;
			a = sArgs.erase(a);
		}
		else if(*a == "-s") {
			passed_seed = true;
			a = sArgs.erase(a);
			try {
				seed = std::stoll(*a);
				a = sArgs.erase(a);
			}
			catch(...) {
				std::cout << "The seed parameter was either not present or invalid." << std::endl;
				return EXIT_FAILURE;
			}
		}
		else if(*a == "-t") {
			ignore_timeout = true;
			a = sArgs.erase(a);
		}
		else a++;
	}

	if(!passed_dimensions) {
		std::string in;
		std::cout << "Please enter the width of the map: ";
		std::getline(std::cin, in);
		while(true) {
			try{
				mapWidth = std::stoi(in);
				break;
			}
			catch(std::exception e) {
				std::cout << "That isn't a valid input. Please enter a positive integer width of the map: ";
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
				std::cout << "That isn't a valid input. Please enter a positive integer height of the map: ";
				std::getline(std::cin, in);
			}
		}
	}

	if(override_names) {
		if(sArgs.size() < 4 || sArgs.size() % 2 != 0) {
			std::cout << "Invalid player parameters (inferred from #) - couldn't start game." << std::endl;
			return EXIT_FAILURE;
		}
		try {
			names = new std::vector<std::string>();
			while(!sArgs.empty()) {
				networking.startAndConnectBot(sArgs.front());
				sArgs.pop_front();
				names->push_back(sArgs.front());
				sArgs.pop_front();
			}
		}
		catch(...) {
			std::cout << "Invalid player parameters - couldn't start game." << std::endl;
			return EXIT_FAILURE;
		}
	}
	else {
		if(sArgs.size() < 2) {
			std::cout << "Invalid player parameters (inferred from #) - couldn't start game." << std::endl;
			return EXIT_FAILURE;
		}
		try {
			while(!sArgs.empty()) {
				std::cout << sArgs.front() << std::endl;
				networking.startAndConnectBot(sArgs.front());
				sArgs.pop_front();
			}
		}
		catch(...) {
			std::cout << "Invalid  playerparameters - couldn't start game." << std::endl;
			return EXIT_FAILURE;
		}
	}

	//Create game. Null parameters will be ignored.
	my_game = new Halite(mapWidth, mapHeight, seed, networking, names);

	std::string filename = "Replays/" + std::to_string(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock().now().time_since_epoch()).count()) + ".hlt";

	GameStatistics stats = my_game->runGame();

	try{
		my_game->output(filename);
	}
	catch(std::runtime_error & e) {
		filename = filename.substr(8);
		if(!quiet_output) {
			std::cout << "Map seed is " << seed << std::endl << "Opening a file at " << filename << std::endl;
		}
		my_game->output(filename);
	}

	std::string victoryOut;
	if(quiet_output) {
		std::cout << filename << ' ' << seed << std::endl << stats;
	}
	else {
		for(unsigned int a = 0; a < stats.player_statistics.size(); a++) std::cout << "Player #" << stats.player_statistics[a].tag << ", " << my_game->getName(stats.player_statistics[a].tag) << ", came in rank #" << stats.player_statistics[a].rank << "!\n";
	}

	delete my_game;

	if(watch_game) {
#ifdef _WIN32
		std::string command = ".\\visualizer " + filename;
#else
		std::string command = "./visualizer " + filename;
#endif
		system(command.c_str());
	}

	return 0;
}

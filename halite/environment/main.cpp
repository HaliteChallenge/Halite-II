#include <iostream>
#include <cctype>
#include <chrono>
#include <list>
#include <string.h>

#include "core/Halite.hpp"

bool program_output_style = false, watch_game = false, server_version = false;
Halite * my_game; //Is a pointer to avoid problems with assignment, dynamic memory, and default constructors.

int main(int argc, char* args[]) {
	srand(time(NULL));

	if(argc > 1) {
		std::list<std::string> sArgs;
		for(int a = 1; a < argc; a++) sArgs.push_back(args[a]);

		for(auto a = sArgs.begin(); a != sArgs.end();) {
			if(*a == "-w") {
				watch_game = true;
				a = sArgs.erase(a);
			}
			else if(*a == "-q") {
				program_output_style = true;
				a = sArgs.erase(a);
			}
			else if(*a == "-s") {
				server_version = true;
				a = sArgs.erase(a);
			}
			else a++;
		}

		unsigned short mapWidth, mapHeight;
		Networking networking;

		try {
			mapWidth = std::stoi(sArgs.front());
			sArgs.pop_front();
			mapHeight = std::stoi(sArgs.front());
			sArgs.pop_front();
		}
		catch(...) {
			std::cout << "Invalid map width and/or height - couldn't start game.\n";
			return EXIT_FAILURE;
		}

		if(server_version) {
			try {
				std::vector<std::string> names;
				while(!sArgs.empty()) {
					networking.startAndConnectBot(sArgs.front());
					sArgs.pop_front();
					names.push_back(sArgs.front());
					sArgs.pop_front();
				}
				my_game = new Halite(mapWidth, mapHeight, networking, names);
			}
			catch(...) {
				std::cout << "[Server Version] Invalid parameters - couldn't start game.\n";
				return EXIT_FAILURE;
			}
		}
		else {
			try {
				while(!sArgs.empty()) {
					std::cout << sArgs.front() << std::endl;
					networking.startAndConnectBot(sArgs.front());
					sArgs.pop_front();
				}
				my_game = new Halite(mapWidth, mapHeight, networking);
			}
			catch(...) {
				std::cout << "[Local Version] Invalid parameters - couldn't start game.\n";
				return EXIT_FAILURE;
			}
		}
	}
	//The programs arguments were not passed in the run command.
	//Instead, we will ask the user for them
	else {
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
	catch(std::runtime_error & e) {
		filename = filename.substr(8);
		if(!program_output_style) std::cout << "Opening a file at " << filename << std::endl;
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

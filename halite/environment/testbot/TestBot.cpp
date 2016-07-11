/*#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <cstdlib>
#include <ctime>
#include <stdio.h>
#include <time.h>
#include <set>
#include <fstream>
#ifdef _WIN32
	#include <windows.h>
	#include <direct.h>
#else
	#include <unistd.h>
#endif

#include "hlt.hpp"
#include "Networking.hpp"

int main2(int argc, const char ** argv) { //Ignore as main until needed.

	std::string loc(argv[0]);
	std::replace(loc.begin(), loc.end(), '\\', '/');
	loc = loc.substr(0, loc.find_last_of('/'));
	_chdir(loc.c_str());

	srand(time(NULL));

	std::cout.sync_with_stdio(0);

	unsigned char my_tag;
	hlt::Map present_map;
	getInit(my_tag, present_map);

	std::string s;
	std::ifstream in("Responses.txt");
	if(!in.is_open()) sendInitResponse("TestBot - Couldn't Open File MORECHARACTERS");
	else {
		std::getline(in, s);
		sendInitResponse(s + std::to_string(my_tag));
	}

	while(true) {
		getFrame(present_map);

		std::getline(in, s);

		detail::sendString(s);
	}

	return 0;
}*/
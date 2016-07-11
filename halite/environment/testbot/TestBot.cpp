#include <stdlib.h>
#include <time.h>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <set>
#include <fstream>
#include <unistd.h>

#include "hlt.hpp"
#include "Networking.hpp"

int main2(int argc, const char ** argv) { //Ignore as main until needed.

	chdir(argv[0]); //Set working directory

	srand(time(NULL));

	std::cout.sync_with_stdio(0);

	unsigned char my_tag;
	hlt::Map present_map;
	getInit(my_tag, present_map);

	std::ifstream in("Responses.txt");
	std::string s;
	std::getline(in, s);

	sendInitResponse(s + std::to_string(my_tag));

	while(true) {
		getFrame(present_map);

		std::getline(in, s);

		detail::sendString(s);
	}

	return 0;
}

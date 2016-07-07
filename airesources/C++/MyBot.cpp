#include <stdlib.h>
#include <time.h>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <set>

#include "hlt.hpp"
#include "Networking.hpp"

int main() {
	srand(time(NULL));

	std::cout.sync_with_stdio(0);

	unsigned char my_tag;
	hlt::Map present_map;
	getInit(my_tag, present_map);
	sendInitResponse("MyBot");

	std::set<hlt::Move> moves;
	while(true) {
		moves.clear();

		getFrame(present_map);

		for(unsigned short a = 0; a < present_map.map_height; a++) {
			for(unsigned short b = 0; b < present_map.map_width; b++) {
				if (present_map.getSite({b, a}, STILL).owner == my_tag) {
					moves.insert({ { b, a }, (unsigned char)(rand() % 5)});
				}
			}
		}

		sendFrame(moves);
	}

	return 0;
}

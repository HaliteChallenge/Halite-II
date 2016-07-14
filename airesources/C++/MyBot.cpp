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

	unsigned char myID;
	hlt::GameMap presentMap;
	getInit(myID, presentMap);
	sendInitResponse("MyBot");

	std::set<hlt::Move> moves;
	while(true) {
		moves.clear();

		getFrame(presentMap);

		for(unsigned short a = 0; a < presentMap.height; a++) {
			for(unsigned short b = 0; b < presentMap.width; b++) {
				if (presentMap.getSite({b, a}).owner == myID) {
					moves.insert({ { b, a }, (unsigned char)(rand() % 5)});
				}
			}
		}

		sendFrame(moves);
	}

	return 0;
}

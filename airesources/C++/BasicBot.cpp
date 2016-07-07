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
	sendInitResponse("BasicC++Bot");

	std::set<hlt::Move> moves;
	while(true) {
		moves.clear();

		getFrame(present_map);

		for(unsigned short y = 0; y < present_map.map_height; y++) {
			for(unsigned short x = 0; x < present_map.map_width; x++) {
				hlt::Site site = present_map.contents[y][x];
				if (site.owner == my_tag) {
					unsigned char moveDirection = (unsigned char)(rand() % 5);
					if(site.strength < site.production*5) {
						moveDirection = STILL;
					} else {
						for(int d : CARDINALS) {
							if(present_map.getSite({x, y}, d).owner != my_tag) {
								moveDirection = d;
							}
						}
					}
					moves.insert({{x, y}, moveDirection});
				}
			}
		}
		sendFrame(moves);
	}

	return 0;
}

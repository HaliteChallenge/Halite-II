#ifndef HLT_H
#define HLT_H

#include <list>
#include <vector>
#include <random>

#define STILL 0
#define NORTH 1
#define EAST 2
#define SOUTH 3
#define WEST 4

#define DIRECTIONS { STILL, NORTH, EAST, SOUTH, WEST }
#define CARDINALS { NORTH, EAST, SOUTH, WEST }

namespace hlt {
	unsigned char oppositeDirection(unsigned char d) {
		if(d == NORTH) return SOUTH;
		if(d == EAST) return WEST;
		if(d == SOUTH) return NORTH;
		if(d == WEST) return EAST;
		return 0;
	}

	struct Location{
		unsigned short x, y;
	};
	static bool operator<(const Location& l1, const Location& l2) {
		return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
	}

	struct Site{
		unsigned char owner;
		unsigned char strength;
		unsigned char production;
	};

	class Map{
	public:
		std::vector< std::vector<Site> > contents;
		unsigned short map_width, map_height; //Number of rows & columns, NOT maximum index.

		Map() {
			map_width = 0;
			map_height = 0;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0, 0 }));
		}
		Map(const Map &otherMap) {
			map_width = otherMap.map_width;
			map_height = otherMap.map_height;
			contents = otherMap.contents;
		}
		Map(int width, int height) {
			map_width = width;
			map_height = height;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0, 0 }));
		}

		bool inBounds(Location l) {
			return l.x < map_width && l.y < map_height;
		}
		double getDistance(Location l1, Location l2) {
			short dx = abs(l1.x - l2.x), dy = abs(l1.y - l2.y);
			if(dx > map_width / 2) dx = map_width - dx;
			if(dy > map_height / 2) dy = map_height - dy;
			return dx + dy;
		}
		double getAngle(Location l1, Location l2) {
			short dx = l2.x - l1.x, dy = l2.y - l1.y;
			if(dx > map_width - dx) dx -= map_width;
			else if(-dx > map_width + dx) dx += map_width;
			if(dy > map_height - dy) dy -= map_height;
			else if(-dy > map_height + dy) dy += map_height;
			return atan2(dy, dx);
		}

		Location getLocation(Location l, unsigned char direction = STILL) {
			if(direction != STILL) {
				if(direction == NORTH) {
					if(l.y == 0) l.y = map_height - 1;
					else l.y--;
				}
				else if(direction == EAST) {
					if(l.x == map_width - 1) l.x = 0;
					else l.x++;
				}
				else if(direction == SOUTH) {
					if(l.y == map_height - 1) l.y = 0;
					else l.y++;
				}
				else if(direction == WEST) {
					if(l.x == 0) l.x = map_width - 1;
					else l.x--;
				}
			}
			return l;
		}
		Site& getSite(Location l, unsigned char direction = STILL) {
			l = getLocation(l, direction);
			return contents[l.y][l.x];
		}
	};
}

#endif

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

namespace hlt
{
	enum MessageType { ATTACK, STOP_ATTACK };
	struct Message {
		MessageType type;
		int senderID, recipientID, targetID;
	};
	struct Location
	{
		unsigned short x, y;
	};
	static bool operator<(const Location& l1, const Location& l2)
	{
		return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
	}

	struct Site
	{
		unsigned char owner;
		unsigned char strength;
		unsigned char production;
	};

	class Map
	{
	public:
		std::vector< std::vector<Site> > contents;
		unsigned short map_width, map_height; //Number of rows & columns, NOT maximum index.

		Map()
		{
			map_width = 0;
			map_height = 0;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0 }));
		}
		Map(const Map &otherMap)
		{
			map_width = otherMap.map_width;
			map_height = otherMap.map_height;
			contents = otherMap.contents;
		}

		bool inBounds(Location l)
		{
			return l.x < map_width && l.y < map_height;
		}
		float getDistance(Location l1, Location l2)
		{
			short dx = abs(l1.x - l2.x), dy = abs(l1.y - l2.y);
			if(dx > map_width / 2) dx = map_width - dx;
			if(dy > map_height / 2) dy = map_height - dy;
			return sqrt((dx*dx) + (dy*dy));
		}
		float getAngle(Location l1, Location l2)
		{
			short dx = l2.x - l1.x, dy = l2.y - l1.y;
			if(dx > map_width - dx) dx -= map_width;
			else if(-dx > map_width + dx) dx += map_width;
			if(dy > map_height - dy) dy -= map_height;
			else if(-dy > map_height + dy) dy += map_height;
			return atan2(dy, dx);
		}

		Location getLocation(Location l, unsigned char direction)
		{
			if(direction != STILL)
			{
				if(direction == NORTH)
				{
					if(l.y == 0) l.y = map_height - 1;
					else l.y--;
				}
				else if(direction == EAST)
				{
					if(l.x == map_width - 1) l.x = 0;
					else l.x++;
				}
				else if(direction == SOUTH)
				{
					if(l.y == map_height - 1) l.y = 0;
					else l.y++;
				}
				else if(direction == WEST)
				{
					if(l.x == 0) l.x = map_width - 1;
					else l.x--;
				}
			}
			return l;
		}
		Site& getSite(Location l, unsigned char direction)
		{
			l = getLocation(l, direction);
			return contents[l.y][l.x];
		}
	};

	struct Move
	{
		Location loc; unsigned char dir;
	};
	static bool operator<(const Move& m1, const Move& m2)
	{
		unsigned int l1Prod = ((m1.loc.x + m1.loc.y)*((unsigned int)m1.loc.x + m1.loc.y + 1) / 2) + m1.loc.y, l2Prod = ((m2.loc.x + m2.loc.y)*((unsigned int)m2.loc.x + m2.loc.y + 1) / 2) + m2.loc.y;
		return ((l1Prod + m1.dir)*(l1Prod + m1.dir + 1) / 2) + m1.dir < ((l2Prod + m2.dir)*(l2Prod + m2.dir + 1) / 2) + m2.dir;
	}
}

#endif
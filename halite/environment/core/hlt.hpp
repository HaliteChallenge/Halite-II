#pragma once

#include <list>
#include <vector>
#include <random>
#include <chrono>
#include <iostream>

#define STILL 0
#define NORTH 1
#define EAST 2
#define SOUTH 3
#define WEST 4

struct Color
{
	float r, g, b;
};

namespace hlt
{
	static bool isValidDirection(unsigned char d)
	{
		return d >= STILL && d <= WEST;
	}
	static unsigned char oppositeDirection(unsigned char d)
	{
		if(d == STILL) return STILL;
		if(d % 2 == 1) return 4 - d;
		return 6 - d;
	}

	enum MessageType {ATTACK, STOP_ATTACK};
	struct Message {
		MessageType type;
		int senderID, recipientID, targetID;
	};
	struct Location
	{
		unsigned short x, y;
	};
	static bool operator<(const Location & l1, const Location & l2)
	{
		return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
	}
	static bool operator==(const Location & l1, const Location & l2)
	{
		return l1.x == l2.x && l1.y == l2.y;
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
		unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

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
		Map(short width, short height, unsigned char numberOfPlayers)
		{
			//Find number closest to square that makes the match symmetric.
			int dw = sqrt(numberOfPlayers);
			while(numberOfPlayers % dw != 0) dw--;
			int dh = numberOfPlayers / dw;

			//Figure out chunk width and height accordingly.
			//Matches width and height as closely as it can, but is not guaranteed to match exactly.
			//It is guaranteed to be smaller if not the same size, however.
			int cw = width / dw;
			int ch = height / dh;
			//We're temporarily setting the map width and height to be that of the chunks,
			//which lets us use the getLocation function with the chunks.
			//We'll set it again later.
			map_width = cw;
			map_height = ch;

			//Pseudorandom number generator.
			std::mt19937 prg(std::chrono::system_clock::now().time_since_epoch().count());
			std::uniform_real_distribution<double> urd(0.0, 1.0);
			//Generate the chunk with random values:
			std::vector< std::vector<float> > chunk(ch, std::vector<float>(cw));
			for(int a = 0; a < ch; a++) {
				for(int b = 0; b < cw; b++) {
					float d = urd(prg);
					d = pow(d, 5) * 32;
					chunk[a][b] = d;
				}
			}
			//Iterate over the map 6 times (found by experiment) to produce the chunk
			for(int a = 0; a < 6; a++) {
				std::vector< std::vector<float> > newChunk = std::vector< std::vector<float> >(ch, std::vector<float>(cw, 0));
				const double OWN_WEIGHT = 0.667;
				for(unsigned short y = 0; y < ch; y++) {
					for(unsigned short x = 0; x < cw; x++) {
						Location l = { x, y };
						Location n = getLocation(l, NORTH), e = getLocation(l, EAST), s = getLocation(l, SOUTH), w = getLocation(l, WEST);
						newChunk[l.y][l.x] = OWN_WEIGHT * chunk[l.y][l.x];
						newChunk[l.y][l.x] += (1 - OWN_WEIGHT) * chunk[n.y][n.x] / 4;
						newChunk[l.y][l.x] += (1 - OWN_WEIGHT) * chunk[e.y][e.x] / 4;
						newChunk[l.y][l.x] += (1 - OWN_WEIGHT) * chunk[s.y][s.x] / 4;
						newChunk[l.y][l.x] += (1 - OWN_WEIGHT) * chunk[w.y][w.x] / 4;
						newChunk[l.y][l.x] -= 1.5 * urd(prg);
						if(newChunk[l.y][l.x] < 0) newChunk[l.y][l.x] = 1;
					}
				}
				chunk = newChunk;
			}

			map_width = cw * dw;
			map_height = ch * dh;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0, 0 }));

			//Fill in with chunks.
			for(int a = 0; a < dh; a++) {
				for(int b = 0; b < dw; b++) {
					for(int c = 0; c < ch; c++) {
						for(int d = 0; d < cw; d++) {
							contents[a * ch + c][b * cw + d].production = round(chunk[c][d]); //Set production values.
							contents[a * ch + c][b * cw + d].strength = 8 * ((urd(prg) / 2) + 0.75) * round(chunk[c][d]);
						}
					}
					contents[a * ch + ch / 2][b * cw + cw / 2].owner = a * dw + b + 1; //Set owners.
					contents[a * ch + ch / 2][b * cw + cw / 2].strength = 255; //Set strengths
				}
			}
		}

		bool inBounds(Location l)
		{
			return l.x < map_width && l.y < map_height;
		}
		float getDistance(Location l1, Location l2)
		{
			short dx = abs(l1.x - l2.x), dy = abs(l1.y - l2.y);
			if (dx > map_width / 2) dx = map_width - dx;
			if (dy > map_height / 2) dy = map_height - dy;
			return sqrt((dx*dx) + (dy*dy));
		}
		float getAngle(Location l1, Location l2)
		{
			short dx = l2.x - l1.x, dy = l2.y - l1.y;
			if (dx > map_width - dx) dx -= map_width;
			else if (-dx > map_width + dx) dx += map_width;
			if (dy > map_height - dy) dy -= map_height;
			else if (-dy > map_height + dy) dy += map_height;
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

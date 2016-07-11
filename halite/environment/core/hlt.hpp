#pragma once

#include <list>
#include <vector>
#include <random>
#include <functional>
#include <chrono>
#include <iostream>

#define STILL 0
#define NORTH 1
#define EAST 2
#define SOUTH 3
#define WEST 4

struct Color{
	float r, g, b;
};

namespace hlt{
	struct Location{
		unsigned short x, y;
	};
	static bool operator<(const Location & l1, const Location & l2) {
		return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
	}
	static bool operator==(const Location & l1, const Location & l2) {
		return l1.x == l2.x && l1.y == l2.y;
	}

	struct Site {
		unsigned char owner;
		unsigned char strength;
		unsigned char production;
	};

	class Map{
	public:
		std::vector< std::vector<Site> > contents;
		unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

		Map() {
			map_width = 0;
			map_height = 0;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0 }));
		}
		Map(const Map &otherMap) {
			map_width = otherMap.map_width;
			map_height = otherMap.map_height;
			contents = otherMap.contents;
		}
		Map(short width, short height, unsigned char numberOfPlayers) {
			//Find number closest to square that makes the match symmetric.
			int dw = sqrt(numberOfPlayers);
			while(numberOfPlayers % dw != 0) dw--;
			int dh = numberOfPlayers / dw;

			//Figure out chunk width and height accordingly.
			//Matches width and height as closely as it can, but is not guaranteed to match exactly.
			//It is guaranteed to be smaller if not the same size, however.
			int cw = width / dw;
			int ch = height / dh;

			//Pseudorandom number generator.
			std::mt19937 prg(std::chrono::system_clock::now().time_since_epoch().count());
			std::uniform_real_distribution<double> urd(0.0, 1.0);

			const std::function<double()> rud = [&]() -> double { return urd(prg); };

			class Region {
			private:
				double factor;
			public:
				std::vector< std::vector<Region * > > children; //Tries to make it 4x4.
				Region(int _w, int _h, double _min, double _max, const std::function<double()> & _rud) {
					factor = pow(_rud(), 2) * (_max - _min) + _min;
					children.clear();
					const int CHUNK_SIZE = 4;
					if(_w == 1 && _h == 1) return;
					int cw = _w / CHUNK_SIZE, ch = _h / CHUNK_SIZE;
					int difW = _w - CHUNK_SIZE * cw, difH = _h - CHUNK_SIZE * ch;
					for(int a = 0; a < CHUNK_SIZE; a++) {
						int tch = a < difH ? ch + 1 : ch;
						if(tch > 0) {
							children.push_back(std::vector<Region * >());
							for(int b = 0; b < CHUNK_SIZE; b++) {
								int tcw = b < difW ? cw + 1 : cw;
								if(tcw > 0) {
									children.back().push_back(new Region(tcw, tch, _min, _max, _rud));
								}
							}
						}
					}
					const double OWN_WEIGHT = 0.75;
					for(int a = 0; a < 2; a++) { //2 iterations found by experiment.
						for(int a = 0; a < children.size(); a++) {
							int mh = a - 1, ph = a + 1;
							if(mh < 0) mh += children.size();
							if(ph == children.size()) ph = 0;
							for(int b = 0; b < children.front().size(); b++) {
								int mw = b - 1, pw = b + 1;
								if(mw < 0) mw += children.front().size();
								if(pw == children.front().size()) pw = 0;
								children[a][b]->factor *= OWN_WEIGHT;
								children[a][b]->factor += children[mh][b]->factor * (1 - OWN_WEIGHT) / 4;
								children[a][b]->factor += children[ph][b]->factor * (1 - OWN_WEIGHT) / 4;
								children[a][b]->factor += children[a][mw]->factor * (1 - OWN_WEIGHT) / 4;
								children[a][b]->factor += children[a][pw]->factor * (1 - OWN_WEIGHT) / 4;
							}
						}
					}
				 };
				std::vector< std::vector<double> > getFactors() {
					if(children.size() == 0) return std::vector< std::vector<double> >(1, std::vector<double>(1, factor));
					std::vector< std::vector< std::vector< std::vector<double> > > > childrenFactors(children.size(), std::vector< std::vector< std::vector<double> > >(children.front().size()));
					for(int a = 0; a < children.size(); a++) {
						for(int b = 0; b < children.front().size(); b++) {
							childrenFactors[a][b] = children[a][b]->getFactors();
						}
					}
					int width = 0, height = 0;
					for(int a = 0; a < children.size(); a++) height += childrenFactors[a].front().size();
					for(int b = 0; b < children.front().size(); b++) width += childrenFactors.front()[b].front().size();
					std::vector< std::vector<double> > factors(height, std::vector<double>(width));
					int x = 0, y = 0;
					for(int my = 0; my < children.size(); my++) {
						for(int iy = 0; iy < childrenFactors[my].front().size(); iy++) {
							for(int mx = 0; mx < children.front().size(); mx++) {
								for(int ix = 0; ix < childrenFactors.front()[mx].front().size(); ix++) {
									factors[y][x] = childrenFactors[my][mx][iy][ix] * factor;
									x++;
								}
							}
							y++;
							x = 0;
						}
					}
					return factors;
				}
				~Region() { for(auto a = children.begin(); a != children.end(); a++) for(auto b = a->begin(); b != a->end(); b++) delete *b; }
			};

			//For final iteration
			const double OWN_WEIGHT = 0.66667;

			Region prodRegion(cw, ch, 0.5, 3, rud);
			std::vector< std::vector<double> > prodChunk = prodRegion.getFactors();

			//Iterate this region as well to produce better caverns:
			for(int a = 0; a < 5; a++) { // 5 iterations found by experiment.
				for(int a = 0; a < prodChunk.size(); a++) {
					int mh = a - 1, ph = a + 1;
					if(mh < 0) mh += prodChunk.size();
					if(ph == prodChunk.size()) ph = 0;
					for(int b = 0; b < prodChunk.front().size(); b++) {
						int mw = b - 1, pw = b + 1;
						if(mw < 0) mw += prodChunk.front().size();
						if(pw == prodChunk.front().size()) pw = 0;
						prodChunk[a][b] *= OWN_WEIGHT;
						prodChunk[a][b] += prodChunk[mh][b] * (1 - OWN_WEIGHT) / 4;
						prodChunk[a][b] += prodChunk[ph][b] * (1 - OWN_WEIGHT) / 4;
						prodChunk[a][b] += prodChunk[a][mw] * (1 - OWN_WEIGHT) / 4;
						prodChunk[a][b] += prodChunk[a][pw] * (1 - OWN_WEIGHT) / 4;
					}
				}
			}

			Region strengthRegion(cw, ch, 0.4, 2.25, rud);
			std::vector< std::vector<double> > strengthChunk = strengthRegion.getFactors();

			//Iterate this region as well to produce better caverns:
			for(int a = 0; a < 3; a++) { // 3 iterations found by experiment.
				for(int a = 0; a < strengthChunk.size(); a++) {
					int mh = a - 1, ph = a + 1;
					if(mh < 0) mh += strengthChunk.size();
					if(ph == strengthChunk.size()) ph = 0;
					for(int b = 0; b < strengthChunk.front().size(); b++) {
						int mw = b - 1, pw = b + 1;
						if(mw < 0) mw += strengthChunk.front().size();
						if(pw == strengthChunk.front().size()) pw = 0;
						strengthChunk[a][b] *= OWN_WEIGHT;
						strengthChunk[a][b] += strengthChunk[mh][b] * (1 - OWN_WEIGHT) / 4;
						strengthChunk[a][b] += strengthChunk[ph][b] * (1 - OWN_WEIGHT) / 4;
						strengthChunk[a][b] += strengthChunk[a][mw] * (1 - OWN_WEIGHT) / 4;
						strengthChunk[a][b] += strengthChunk[a][pw] * (1 - OWN_WEIGHT) / 4;
					}
				}
			}

			map_width = cw * dw;
			map_height = ch * dh;
			contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width, { 0, 0, 0 }));

			//Fill in with chunks.
			const int MED_PROD = 1, MED_STR = 50;
			bool reflectVertical = dh % 2 == 0, reflectHorizontal = dw % 2 == 0;
			for(int a = 0; a < dh; a++) {
				for(int b = 0; b < dw; b++) {
					for(int c = 0; c < ch; c++) {
						for(int d = 0; d < cw; d++) {
							int cc = reflectVertical && a % 2 != 0 ? ch - c - 1 : c, dd = reflectHorizontal && b % 2 != 0 ? cw - d - 1 : d;
							contents[a * ch + c][b * cw + d].production = round(MED_PROD * prodChunk[cc][dd]); //Set production values.
							contents[a * ch + c][b * cw + d].strength = round(MED_STR * strengthChunk[cc][dd]);
						}
					}
					contents[a * ch + ch / 2][b * cw + cw / 2].owner = a * dw + b + 1; //Set owners.
					contents[a * ch + ch / 2][b * cw + cw / 2].strength = 255; //Set strengths
				}
			}
		}

		bool inBounds(Location l) const {
			return l.x < map_width && l.y < map_height;
		}
		float getDistance(Location l1, Location l2) const {
			short dx = abs(l1.x - l2.x), dy = abs(l1.y - l2.y);
			if (dx > map_width / 2) dx = map_width - dx;
			if (dy > map_height / 2) dy = map_height - dy;
			return sqrt((dx*dx) + (dy*dy));
		}
		float getAngle(Location l1, Location l2) const {
			short dx = l2.x - l1.x, dy = l2.y - l1.y;
			if (dx > map_width - dx) dx -= map_width;
			else if (-dx > map_width + dx) dx += map_width;
			if (dy > map_height - dy) dy -= map_height;
			else if (-dy > map_height + dy) dy += map_height;
			return atan2(dy, dx);
		}

		Location getLocation(Location l, unsigned char direction) const {
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
		Site& getSite(Location l, unsigned char direction) {
			l = getLocation(l, direction);
			return contents[l.y][l.x];
		}
	};

	struct Move {
		Location loc; unsigned char dir;
	};
	static bool operator<(const Move& m1, const Move& m2) {
		unsigned int l1Prod = ((m1.loc.x + m1.loc.y)*((unsigned int)m1.loc.x + m1.loc.y + 1) / 2) + m1.loc.y, l2Prod = ((m2.loc.x + m2.loc.y)*((unsigned int)m2.loc.x + m2.loc.y + 1) / 2) + m2.loc.y;
		return ((l1Prod + m1.dir)*(l1Prod + m1.dir + 1) / 2) + m1.dir < ((l2Prod + m2.dir)*(l2Prod + m2.dir + 1) / 2) + m2.dir;
	}
}

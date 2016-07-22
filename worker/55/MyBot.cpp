#include <stdlib.h>
#include <time.h>
#include <cstdlib>
#include <ctime>
#include <queue>
#include <list>
#include <time.h>
#include <set>
#include <map>
#include <fstream>

#include "hlt.hpp"
#include "networking.hpp"

int main() {
	srand(time(NULL));

	std::cout.sync_with_stdio(0);

	short turn = 1;

	unsigned char my_tag;
	hlt::Map present_map;
	getInit(my_tag, present_map);
	sendInitResponse("Sdyx-Bot" + std::to_string(my_tag));

	std::ofstream file("sydxdbg" + std::to_string(my_tag) + ".log");

	std::map<hlt::Location, unsigned char> moves;
	while(true) {
		moves.clear();

		file << "Entering turn #" << turn << std::endl;

		getFrame(present_map);

		std::vector< std::vector<int> > distances(present_map.map_height, std::vector<int>(present_map.map_width, 0)); //Distances from a border. My squares should have values >= 1, and map squares <= -1.
		auto dAt = [&](const hlt::Location & l) -> int & { return distances[l.y][l.x]; };
		std::vector< std::vector<double> > prodvals(present_map.map_height, std::vector<double>(present_map.map_width, -1)); //Distances from a border. My squares should have values >= 1, and map squares <= -1.
		auto vAt = [&](const hlt::Location & l) -> double & { return prodvals[l.y][l.x]; };

		std::set<hlt::Location> externFront, internFront;

		//Start by moving every piece with strength 0 to remain still. This should save time in the future and prevent bad moves.
		for(unsigned short y = 0; y < present_map.map_height; y++) {
			for(unsigned short x = 0; x < present_map.map_width; x++) {
				hlt::Location l = { x, y };
				hlt::Site & site = present_map.getSite(l);
				if(site.owner == my_tag && site.strength == 0) {
					moves.insert({ l, STILL });
				}
			}
		}

		hlt::Location center; //Mark this as the center piece. Later, during combat, we'll move internal pieces away from this one.
		for(unsigned short y = 0; y < present_map.map_height; y++) {
			for(unsigned short x = 0; x < present_map.map_width; x++) {
				hlt::Location l = { x, y };
				hlt::Site & site = present_map.getSite(l);
				if(site.owner == my_tag) {
					bool hasBorder = false;
					for(int d : CARDINALS) {
						if(present_map.getSite(l, d).owner != my_tag) {
							hasBorder = true;
							break;
						}
					}
					if(hasBorder) internFront.insert(l);
				}
				else {
					bool hasBorder = false;
					for(int d : CARDINALS) {
						if(present_map.getSite(l, d).owner == my_tag) {
							hasBorder = true;
							break;
						}
					}
					if(hasBorder) {
						dAt(l) = -1; //This is necessary so that the internal bfs doesn't use any external squares.
						//Let's iterate through the map and find the value of the square through using a sort of pull.
						// - Squares with higher production are better.
						// - Squares near a lot of my strength are better.
						// - Squares near a lot of enemy strength are worse.
						externFront.insert(l);
						double val = 0;
						for(unsigned short y = 0; y < present_map.map_height; y++) {
							for(unsigned short x = 0; x < present_map.map_width; x++) {
								hlt::Location loc = { x, y };
								double dist = 0.6180399 + present_map.getDistance(l, loc); //Golden ratio, mostly for fun.
								hlt::Site s = present_map.getSite(l);
								double dif = 10 * s.production; //Arbitrary; expect to recoup costs within 16 turns on avg, to be fair. [fut] initialize this value on startup according to the map ratio itself.
								if(s.owner == my_tag) dif = s.strength; 
								else dif -= s.strength;
								dif /= (dist * dist);
								val += dif;
							}
						}
						vAt(l) = val;
					}
				}
			}
		}

		//Get internal distances.
		for(int z = 1; !internFront.empty(); z++) {
			std::set<hlt::Location> newInternFront;
			for(auto a = internFront.begin(); a != internFront.end(); a++) {
				dAt(*a) = z;
				for(int d : CARDINALS) {
					hlt::Location l = present_map.getLocation(*a, d);
					if(dAt(l) == 0 && internFront.count(l) == 0) {
						newInternFront.insert(l);
					}
				}
			}
			if(newInternFront.empty()) center = *(internFront.begin());
			internFront = newInternFront;
		}

		file << "This is how I see the dists of the map:" << std::endl;
		for(unsigned short y = 0; y < present_map.map_height; y++) {
			for(unsigned short x = 0; x < present_map.map_width; x++) {
				int d = dAt({ x, y });
				file << d;
				if(d < -9) file << ' ';
				else if(d < 0 || d > 9) file << "  ";
				else file << "   ";
			}
			file << std::endl;
		}

		//Fill in border with pieces that don't have to worry about combat (pieces without adjacent enemy pieces). [Technically should be 2 dist but I think this will give better behavior].
		auto possibleCombat = [&](const hlt::Location & l) -> bool {
			for(int d: DIRECTIONS) {
				hlt::Location loc = present_map.getLocation(l, d);
				if(present_map.getSite(loc).owner != my_tag && present_map.getSite(loc).owner != 0) return true;
			}
			return false;
		};
		std::vector<hlt::Location> expansionBorder, combatBorder;
		for(auto a = externFront.begin(); a != externFront.end(); a++) {
			if(!possibleCombat(*a)) expansionBorder.push_back(*a);
			else combatBorder.push_back(*a);
		}
		//Sort expansion and combat both by the val.
		std::sort(expansionBorder.begin(), expansionBorder.end(), [&](const hlt::Location & l1, const hlt::Location & l2) -> bool { return vAt(l1) > vAt(l2); });
		std::sort(combatBorder.begin(), combatBorder.end(), [&](const hlt::Location & l1, const hlt::Location & l2) -> bool { return vAt(l1) > vAt(l2); });

		file << "Expansion border has size " << (int)expansionBorder.size() << std::endl;
		file << "Combat border has size " << (int)combatBorder.size() << std::endl;

		//Prioritize combat (do those moves first). We'll devote pieces up to a distance equal to the 2 + size of the border (divided by some factor) away from the square to combat.
		int maxCombatDist = 2 + int(combatBorder.size()) / 4;
		//Only go to the square if the dir is a mod 3.
		for(auto a = combatBorder.begin(); a != combatBorder.end(); a++) {
			std::set<hlt::Location> front;
			for(int d : { NORTH, SOUTH }) {
				hlt::Location loc = present_map.getLocation(*a, d);
				if(present_map.getSite(loc).owner == my_tag) {// && loc.x % 3 == 0) { This helps on the flats, but really hurts everywhere else. It'd be nice to find a way to incorporate it, tho.
					front.insert(*a);
					break;
				}
			}
			for(int d : { EAST, WEST }) {
				hlt::Location loc = present_map.getLocation(*a, d);
				if(present_map.getSite(loc).owner == my_tag) {// && loc.y % 3 == 0) { This helps on the flats, but really hurts everywhere else. It'd be nice to find a way to incorporate it, tho.
					front.insert(*a);
					break;
				}
			}
			if(!front.empty()) { //Start BFS
				for(int z = 0; z < maxCombatDist; z++) {
					std::set<hlt::Location> newFront;
					for(auto b = front.begin(); b != front.end(); b++) {
						for(int d : CARDINALS) {
							hlt::Location l = present_map.getLocation(*a, d);
							if(moves.count(l) == 0 && present_map.getSite(l).strength > (4 + z) * present_map.getSite(l).production) { //4 must be tuned.
								newFront.insert(l);
								moves[l] = hlt::oppositeDirection(d);
							}
						}
					}
					front = newFront;
				}
			}
		}

		//Run bfs until we've satisfied this required strength -> expansion.
		for(auto a = expansionBorder.begin(); a != expansionBorder.end(); a++) {
			int requiredStrength = 6 + 2 * present_map.getSite(*a).strength; //This looks weird but is right. It's because we'll subtract it later when we go through the b loop. 5 is buffer.
			std::vector< std::set<hlt::Location> > bfsPieces(1, std::set<hlt::Location>());
			bfsPieces.back().insert(*a);
			std::vector<int> distProdCounts;
			while(!bfsPieces.back().empty()) {
				std::set<hlt::Location> nextBfsPieces;
				int prodCount = 0;
				for(auto b = bfsPieces.back().begin(); requiredStrength > 0 && b != bfsPieces.back().end(); b++) {
					requiredStrength -= present_map.getSite(*b).strength;
					prodCount += present_map.getSite(*b).production;

					for(int d : CARDINALS) {
						hlt::Location t = present_map.getLocation(*b, d);
						if(present_map.getSite(t).owner == my_tag && moves.count(t) == 0) {
							moves.insert({ t, hlt::oppositeDirection(d) });
							nextBfsPieces.insert(t);
						}
					}
				}
				if(requiredStrength <= 0) break;
				else {
					for(auto b = distProdCounts.begin(); b != distProdCounts.end(); b++) requiredStrength -= *b;
					if(requiredStrength <= 0) break; //Again, if possible use fewer pieces over more moves.
					bfsPieces.push_back(nextBfsPieces);
					distProdCounts.push_back(prodCount);
				}
			}
			for(auto b = bfsPieces.begin(); b != bfsPieces.end() - 1; b++) for(auto c = b->begin(); c != b->end(); c++) moves[*c] = STILL; //Tell pieces which aren't enough yet to stay still.
		}

		//Use gradients from earlier dist calculations to route pieces towards lower dists. Prefer sites with lower strengths.
		for(unsigned short y = 0; y < present_map.map_height; y++) {
			for(unsigned short x = 0; x < present_map.map_width; x++) {
				hlt::Location l = { x, y };
				hlt::Site & site = present_map.getSite(l);
				if(site.owner == my_tag && moves.count(l) == 0) {
					if(site.strength < short(site.production) * 6 && site.strength != 255) {
						moves.insert({ l, STILL });
					}
					else {
						unsigned short dist = dAt(l);
						std::set<unsigned char> acceptableDirections;
						for(int d: CARDINALS) if(dAt(present_map.getLocation(l, d)) < dist) acceptableDirections.insert(d);
						unsigned char lowestStrength = 255, dir = 0;
						for(auto a = acceptableDirections.begin(); a != acceptableDirections.end(); a++) {
							unsigned char str = present_map.getSite(l, *a).strength;
							if(str < lowestStrength) {
								dir = *a;
								lowestStrength = str;
							}
						}
						moves.insert({ l, dir });
					}
				}
			}
		}

		sendFrame(moves);

		turn++;
	}

	return 0;
}

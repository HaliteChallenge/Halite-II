// For debugging maps.

#include "hlt.hpp"
#include <fstream>

const int MAP_WIDTH = 24, MAP_HEIGHT = 24;
const int NUM_PLAYERS = 1;

unsigned char sdh(unsigned char val) {
	if(val < 10) return val + 48;
	else return val + 55;
}

int main2() {
	hlt::Map m(MAP_WIDTH, MAP_HEIGHT, NUM_PLAYERS);
	std::ofstream of("MapDbg.txt");
	for(int a = 0; a < MAP_HEIGHT; a++) {
		for(int b = 0; b < MAP_WIDTH; b++) {
			of << sdh(m.contents[a][b].production) << ' ';
		}
		of << '\n';
	}
	of << "------------------------------------------------------------------------------\n";
	for(int a = 0; a < MAP_HEIGHT; a++) {
		for(int b = 0; b < MAP_WIDTH; b++) {
			of << sdh(m.contents[a][b].strength / 16) << sdh(m.contents[a][b].strength % 16) << ' ';
		}
		of << '\n';
	}
	of << "------------------------------------------------------------------------------\n";
	int maxProd = 0;
	for(int a = 0; a < MAP_HEIGHT; a++) {
		for(int b = 0; b < MAP_WIDTH; b++) {
			if(m.contents[a][b].production > maxProd) maxProd = m.contents[a][b].production;
		}
	}
	std::vector<int> prodCounts(maxProd + 1);
	for(int a = 0; a < MAP_HEIGHT; a++) {
		for(int b = 0; b < MAP_WIDTH; b++) {
			prodCounts[m.contents[a][b].production]++;
		}
	}
	for(int a = 0; a <= maxProd; a++) {
		of << a << ": " << prodCounts[a] << " counts yielding " << prodCounts[a] * a << " total production.\n";
	}
	of << "------------------------------------------------------------------------------\n";
	int chunkProduction = 0, chunkStrength = 0;
	for(int a = 0; a < MAP_HEIGHT; a++) {
		for(int b = 0; b < MAP_WIDTH; b++) {
			chunkProduction += m.contents[a][b].production;
			chunkStrength += m.contents[a][b].strength;
		}
	}
	chunkProduction /= NUM_PLAYERS;
	chunkStrength /= NUM_PLAYERS;
	chunkStrength -= 255;
	of << "Total chunk production is " << chunkProduction << std::endl;
	of << "Total chunk strength is " << chunkStrength << std::endl;
	of.flush();
	return 0;
}
/*
 * This is meant to be a basic startig implementation of map generation.
 * It is by no means perfect or complete.
 * Rather, it will allow testing of the environment and bots until
 * the final version is completed.
 * -Ben
 */

#include <iostream>
#include <algorithm>
#include <vector>
#include <random>
#include <chrono>
#include <fstream>

int width = 0, height = 0;
std::mt19937 prg(std::chrono::system_clock::now().time_since_epoch().count());
std::uniform_real_distribution<double> urd(0.0, 1.0);

struct Tile {
  float threshold;
  float production;
};

struct Location {
  short x, y;
};
Location getNorth(Location l) {
  if(l.y == height - 1) l.y = 0;
  else l.y++;
  return l;
};
Location getEast(Location l) {
  if(l.x == width - 1) l.x = 0;
  else l.x++;
  return l;
};
Location getSouth(Location l) {
  if(l.y == 0) l.y = height - 1;
  else l.y--;
  return l;
};
Location getWest(Location l) {
  if(l.x == 0) l.x = width - 1;
  else l.x--;
  return l;
}
inline Tile & getTile(std::vector< std::vector<Tile> > & map, const Location & l) {
  return map[l.y][l.x];
}

//Uses direct adjacency neighborhood.
std::vector< std::vector<Tile> > iterate(std::vector< std::vector<Tile> > & map) {
  std::vector< std::vector<Tile> > answer = std::vector< std::vector<Tile> >(height, std::vector<Tile>(width, { 0, 0 }));
  const double OWN_WEIGHT = 0.5;
  for(short y = 0; y < height; y++) {
    for(short x = 0; x < width; x++) {
      Location l = { x, y };
      Location n = getNorth(l), s = getSouth(l), e = getEast(l), w = getWest(l);
      Tile & t = getTile(answer, l);
      t.production = OWN_WEIGHT * getTile(map, l).production;
      t.production += (1 - OWN_WEIGHT) * getTile(map, n).production / 4;
      t.production += (1 - OWN_WEIGHT) * getTile(map, e).production / 4;
      t.production += (1 - OWN_WEIGHT) * getTile(map, s).production / 4;
      t.production += (1 - OWN_WEIGHT) * getTile(map, w).production / 4;
      t.production -= urd(prg);
    }
  }
  return answer;
}

int main() {
  std::cin >> width >> height;
  std::vector< std::vector<Tile> > map(height, std::vector<Tile>(width, { 0, 0 }));
  for(int a = 0; a < height; a++) {
    for(int b = 0; b < width; b++) {
      double d = 3 * urd(prg);
      d = d * d + 0.5;
      map[a][b].production = d;
      d *= d / 2;
      map[a][b].threshold = d;
    }
  }
  for(int a = 0; a < 4; a++) {
    map = iterate(map);
  }
  for(short y = 0; y < height; y++) {
    for(short x = 0; x < width; x++) {
      map[y][x].threshold = map[y][x].production * map[y][x].production * (urd(prg) + 1);
    }
  }
  std::ofstream out("out.txt");
  for(int a = 0; a < height; a++) {
    for(int b = 0; b < width; b++) {
      out << int(map[a][b].threshold) << ' ';
    }
    out << std::endl;
  }
  out << "-----------------------------------------------\n";
  for(int a = 0; a < height; a++) {
    for(int b = 0; b < width; b++) {
      out << int(map[a][b].production) << ' ';
    }
    out << std::endl;
  }
  out.flush();
}

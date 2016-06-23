#include "hlt.hpp"
#include <iostream>

int main() {
  const int WIDTH = 44, HEIGHT = 45;
  hlt::Map map(WIDTH, HEIGHT, 4);
  for(int a = 0; a < map.map_height; a++) {
    for(int b = 0; b < map.map_width; b++) {
      std::cout << int(map.contents[a][b].production) << ' ';
    }
    std::cout << std::endl;
  }
  int count[20];
  for(int a = 0; a < 20; a++) count[a] = 0;
  for(int a = 0; a < map.map_height; a++) {
    for(int b = 0; b <  map.map_width; b++) {
      if(map.contents[a][b].production != 0) count[map.contents[a][b].production] += map.contents[a][b].production;
      else count[0]++;
    }
  }
  std::cout << 0 << " -> [" << count[0] << ']' << std::endl;
  for(int a = 1; a < 20; a++) {
    std::cout << a << " -> " << count[a] << std::endl;
  }
}

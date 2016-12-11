#pragma once

#include <list>
#include <vector>
#include <random>
#include <functional>
#include <iostream>
#include <fstream>
#include <assert.h>

#define STILL 0
#define NORTH 1
#define EAST 2
#define SOUTH 3
#define WEST 4

extern bool quiet_output;

struct Color {
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
        Map(short width, short height, unsigned char numberOfPlayers, unsigned int seed) {
            //Pseudorandom number generator.
            std::mt19937 prg(seed);
            std::uniform_real_distribution<double> urd(0.0, 1.0);

            //Decides whether to put more players along the horizontal or the vertical.
            bool preferHorizontal = prg() % 2;

            int dw, dh;
            //Find number closest to square that makes the match symmetric.
            if(preferHorizontal) {
                dh = sqrt(numberOfPlayers);
                while(numberOfPlayers % dh != 0) dh--;
                dw = numberOfPlayers / dh;
            }
            else {
                dw = sqrt(numberOfPlayers);
                while(numberOfPlayers % dw != 0) dw--;
                dh = numberOfPlayers / dw;
            }

            //Figure out chunk width and height accordingly.
            //Matches width and height as closely as it can, but is not guaranteed to match exactly.
            //It is guaranteed to be smaller if not the same size, however.
            int cw = width / dw;
            int ch = height / dh;

            //Ensure that we'll be able to move the tesselation by a uniform amount.
            if(preferHorizontal) while(ch % numberOfPlayers != 0) ch--;
            else while(cw % numberOfPlayers != 0) cw--;

            map_width = cw * dw;
            map_height = ch * dh;

            const std::function<double()> rud = [&]() -> double { return urd(prg); };

            class Region {
            private:
                double factor;
            public:
                std::vector< std::vector<Region * > > children; //Tries to make it 4x4.
                Region(int _w, int _h, const std::function<double()> & _rud) {
                    factor = pow(_rud(), 1.5);
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
                                    children.back().push_back(new Region(tcw, tch, _rud));
                                }
                            }
                        }
                    }
                    const double OWN_WEIGHT = 0.75;
                    for(int z = 0; z < 1; z++) { //1 iterations found by experiment.
                        std::vector< std::vector<double> > blurredFactors(children.size(), std::vector<double>(children.front().size(), 0));
                        for(int a = 0; a < children.size(); a++) {
                            int mh = a - 1, ph = a + 1;
                            if(mh < 0) mh += children.size();
                            if(ph == children.size()) ph = 0;
                            for(int b = 0; b < children.front().size(); b++) {
                                int mw = b - 1, pw = b + 1;
                                if(mw < 0) mw += children.front().size();
                                if(pw == children.front().size()) pw = 0;
                                blurredFactors[a][b] += children[a][b]->factor * OWN_WEIGHT;
                                blurredFactors[a][b] += children[mh][b]->factor * (1 - OWN_WEIGHT) / 4;
                                blurredFactors[a][b] += children[ph][b]->factor * (1 - OWN_WEIGHT) / 4;
                                blurredFactors[a][b] += children[a][mw]->factor * (1 - OWN_WEIGHT) / 4;
                                blurredFactors[a][b] += children[a][pw]->factor * (1 - OWN_WEIGHT) / 4;
                            }
                        }
                         for(int a = 0; a < children.size(); a++) for(int b = 0; b < children.front().size(); b++) children[a][b]->factor = blurredFactors[a][b]; //Set factors.
                    }
                }
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

            Region prodRegion(cw, ch, rud);
            std::vector< std::vector<double> > prodChunk = prodRegion.getFactors();

            Region strengthRegion(cw, ch, rud);
            std::vector< std::vector<double> > strengthChunk = strengthRegion.getFactors();

            struct SiteD {
                unsigned char owner;
                double strength;
                double production;
            };

            //We'll first tesselate the map; we'll apply our various translations and transformations later.
            std::vector< std::vector<SiteD> > tesselation = std::vector< std::vector<SiteD> >(map_height, std::vector<SiteD>(map_width, { 0, 0, 0 }));
            for(int a = 0; a < dh; a++) {
                for(int b = 0; b < dw; b++) {
                    for(int c = 0; c < ch; c++) {
                        for(int d = 0; d < cw; d++) {
                            tesselation[a * ch + c][b * cw + d].production = prodChunk[c][d];
                            tesselation[a * ch + c][b * cw + d].strength = strengthChunk[c][d];
                        }
                    }
                    tesselation[a * ch + ch / 2][b * cw + cw / 2].owner = a * dw + b + 1; //Set owners.
                }
            }

            //We'll now apply the reflections to the map.
            bool reflectVertical = dh % 2 == 0, reflectHorizontal = dw % 2 == 0; //Am I going to reflect in the horizontal vertical directions at all?
            std::vector< std::vector<SiteD> > reflections = std::vector< std::vector<SiteD> >(map_height, std::vector<SiteD>(map_width, { 0, 0, 0 }));
            for(int a = 0; a < dh; a++) {
                for(int b = 0; b < dw; b++) {
                    bool vRef = reflectVertical && a % 2 != 0, hRef = reflectHorizontal && b % 2 != 0; //Do I reflect this chunk at all?
                    for(int c = 0; c < ch; c++) {
                        for(int d = 0; d < cw; d++) {
                            reflections[a * ch + c][b * cw + d] = tesselation[a * ch + (vRef ? ch - c - 1 : c)][b * cw + (hRef ? cw - d - 1 : d)];
                        }
                    }
                }
            }

            
            //Next, let's apply our shifts to create the shifts map.
            std::vector< std::vector<SiteD> > shifts = std::vector< std::vector<SiteD> >(map_height, std::vector<SiteD>(map_width, { 0, 0, 0 }));
            if(numberOfPlayers == 6) shifts = reflections;
            else if(preferHorizontal) {
                int shift = (prg() % dw) * (map_height / dw); //A vertical shift.
                for(int a = 0; a < dh; a++) {
                    for(int b = 0; b < dw; b++) {
                        for(int c = 0; c < ch; c++) {
                            for(int d = 0; d < cw; d++) {
                                shifts[a * ch + c][b * cw + d] = reflections[(a * ch + b * shift + c) % map_height][b * cw + d];
                            }
                        }
                    }
                }
            }
            else {
                int shift = (prg() % dh) * (map_width / dh); //A horizontal shift.
                for(int a = 0; a < dh; a++) {
                    for(int b = 0; b < dw; b++) {
                        for(int c = 0; c < ch; c++) {
                            for(int d = 0; d < cw; d++) {
                                shifts[a * ch + c][b * cw + d] = reflections[a * ch + c][(b * cw + a * shift + d) % map_width];
                            }
                        }
                    }
                }
            }

            //Apply a final blur to create the blur map. This will fix the edges where our transformations have created jumps or gaps.
            const double OWN_WEIGHT = 0.66667;
            std::vector< std::vector<SiteD> > blur = shifts;
            for(int z = 0; z <= 2 * sqrt(map_width * map_height) / 10; z++) {
                std::vector< std::vector<SiteD> > newBlur = blur;
                for(int a = 0; a < map_height; a++) {
                    int mh = a - 1, ph = a + 1;
                    if(mh < 0) mh += map_height;
                    if(ph == map_height) ph = 0;
                    for(int b = 0; b < map_width; b++) {
                        int mw = b - 1, pw = b + 1;
                        if(mw < 0) mw += map_width;
                        if(pw == map_width) pw = 0;
                        newBlur[a][b].production *= OWN_WEIGHT;
                        newBlur[a][b].production += blur[mh][b].production * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].production += blur[ph][b].production * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].production += blur[a][mw].production * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].production += blur[a][pw].production * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].strength *= OWN_WEIGHT;
                        newBlur[a][b].strength += blur[mh][b].strength * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].strength += blur[ph][b].strength * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].strength += blur[a][mw].strength * (1 - OWN_WEIGHT) / 4;
                        newBlur[a][b].strength += blur[a][pw].strength * (1 - OWN_WEIGHT) / 4;
                    }
                }
                blur = newBlur;
            }

            //Let's now normalize the map values.
            double maxProd = 0, maxStr = 0;
            std::vector< std::vector<SiteD> > normalized = blur;
            for(auto a = normalized.begin(); a != normalized.end(); a++) for(auto b = a->begin(); b != a->end(); b++) {
                if(b->production > maxProd) maxProd = b->production;
                if(b->strength > maxStr) maxStr = b->strength;
            }
            for(auto a = normalized.begin(); a != normalized.end(); a++) for(auto b = a->begin(); b != a->end(); b++) {
                b->production /= maxProd;
                b->strength /= maxStr;
            }

            //Finally, fill in the contents vector.
            const int TOP_PROD = prg() % 10 + 6, TOP_STR = prg() % 106 + 150;
            contents = std::vector< std::vector<Site> >(map_height, std::vector<Site>(map_width));
            for(int a = 0; a < map_height; a++) for(int b = 0; b < map_width; b++) {
                contents[a][b].owner = normalized[a][b].owner;
                contents[a][b].strength = round(normalized[a][b].strength * TOP_STR);
                contents[a][b].production = round(normalized[a][b].production * TOP_PROD);
                if(contents[a][b].owner != 0 && contents[a][b].production == 0) contents[a][b].production = 1;
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
        Site& getSite(Location l, unsigned char direction = STILL) {
            l = getLocation(l, direction);
            return contents[l.y][l.x];
        }
    };

    static Map ppmToMap(std::string filename, int numplayers) {
        std::ifstream in(filename, std::ios_base::binary);
        assert(in.is_open());
        int width, height, max_color;
        assert(in.get() == 'P');
        assert(in.get() == '6');
        in >> width >> height >> max_color;
        assert(max_color < 256);
        in.get(); //Get whitespace character.
        Map m;
        m.map_width = width;
        m.map_height = height;
        m.contents = std::vector< std::vector<Site> >(height, std::vector<Site>(width, { 0, 0, 0 }));
        const unsigned char MAX_PROD = 15;//7 + rand() % 9;
        int counter = 1;
        for(int a = 0; a < m.map_height; a++) for(int b = 0; b < m.map_width; b++) {
            if(a == height / 2 && (b % (width / numplayers + 1) == 0)) {
                m.contents[a][b].owner = counter;
                counter++;
            }
            m.contents[a][b].production = (in.get() + in.get() + in.get()) / 765.0 * MAX_PROD;
            m.contents[a][b].strength = 15 * m.contents[a][b].production;
        }
        if(!quiet_output) std::cout << "Loaded ppm" << std::endl;
        return m;
    }
}

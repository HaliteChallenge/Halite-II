//
// Created by David Li on 6/15/17.
//

#include "TilingReflecting.h"

namespace mapgen {
    TilingReflecting::TilingReflecting(unsigned int _seed)
        : Generator(_seed) {}

    auto TilingReflecting::generate(hlt::Map& map,
                                    unsigned int num_players,
                                    unsigned int effective_players) -> void {
        // TODO: revamp algorithm to not need this
        if (effective_players < 2) effective_players = 2;

        // TODO: enforce a minimum map size to make sure we always have room for planets

        //Decides whether to put more players along the horizontal or the vertical.
        bool preferHorizontal = rng() % 2 == 0;

        int dw, dh;
        //Find number closest to square that makes the match symmetric.
        if (preferHorizontal) {
            dh = (int) sqrt(effective_players);
            while (effective_players % dh != 0) dh--;
            dw = effective_players / dh;
        } else {
            dw = (int) sqrt(effective_players);
            while (effective_players % dw != 0) dw--;
            dh = effective_players / dw;
        }

        //Figure out chunk width and height accordingly.
        //Matches width and height as closely as it can, but is not guaranteed to match exactly.
        //It is guaranteed to be smaller if not the same size, however.
        int cw = map.map_width / dw;
        int ch = map.map_height / dh;

        map.map_width = (unsigned short) (cw * dw);
        map.map_height = (unsigned short) (ch * dh);

        // Divide the map into regions for each player

        class Region {
        public:
            unsigned short width;
            unsigned short height;
            unsigned short x;
            unsigned short y;
            int col;
            int row;

            Region(int _col, int _row, int _cw, int _ch) {
                this->x = _col * _cw;
                this->y = _row * _ch;
                this->width = _cw;
                this->height = _ch;
                this->col = _col;
                this->row = _row;
            }

            auto center_x() const -> unsigned short {
                return static_cast<unsigned short>(x + (width / 2));
            }

            auto center_y() const -> unsigned short {
                return static_cast<unsigned short>(y + (height / 2));
            }
        };

        std::vector<Region> regions = std::vector<Region>();
        regions.reserve(effective_players);

        for (int row = 0; row < dh; row++) {
            for (int col = 0; col < dw; col++) {
                regions.push_back(Region(col, row, cw, ch));
            }
        }

        // Center the player's starting ships in each region
        for (hlt::PlayerId playerId = 0; playerId < effective_players;
             playerId++) {
            const auto& region = regions[playerId];

            for (int i = 0; i < 3; i++) {
                // Spread out ships to make it less likely they'll collide
                // in the start
                map.ships[playerId][i].revive(hlt::Location{
                    static_cast<unsigned short>(region.center_x()),
                    static_cast<unsigned short>(region.center_y() - 2 * (i - 1)),
                });
            }
        }

        // Scatter planets throughout all of space, avoiding the starting ships (centers of regions)

        const auto MAX_RADIUS = std::min(cw, ch) / 12;
        std::uniform_int_distribution<unsigned short> uidrw(MAX_RADIUS, cw - MAX_RADIUS);
        std::uniform_int_distribution<unsigned short> uidrh(MAX_RADIUS, ch - MAX_RADIUS);
        std::uniform_int_distribution<unsigned short>
            uidr(3, MAX_RADIUS);
        // TODO: use std::bind here to clean things up
        const auto
            rand_radius = [&]() -> unsigned short { return uidr(rng); };
        const auto rand_region_width = [&]() -> unsigned short { return uidrw(rng); };
        const auto rand_region_height = [&]() -> unsigned short { return uidrh(rng); };

        const auto MAX_PLANETS = effective_players * hlt::GameConstants::get().PLANETS_PER_PLAYER;
        constexpr auto MAX_TRIES = 2500;
        constexpr auto MIN_DISTANCE = 5;

        const auto is_valid_planet_position =
            [&](unsigned short x, unsigned short y, unsigned short r) -> bool {
                if (map.get_distance(
                    { regions[0].center_x(), regions[0].center_y() },
                    { x, y }) < r + MIN_DISTANCE) {
                    return false;
                }

                for (const auto& planet : map.planets) {
                    if (map.get_distance(
                        { planet.location.pos_x, planet.location.pos_y },
                        { x, y }) < r + planet.radius + MIN_DISTANCE) {
                        return false;
                    }
                }

                return true;

            };

        // Generate 4 planets in a region, then mirror that across all regions
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < MAX_TRIES; j++) {
                const auto x = rand_region_width();
                const auto y = rand_region_height();
                const auto r = rand_radius();

                if (!is_valid_planet_position(x, y, r)) {
                    continue;
                }

                for (const auto& region : regions) {
                    map.planets.push_back(hlt::Planet(region.x + x, region.y + y, r));
                }

                break;
            }
        }

        // Try to generate planets on the border of regions, mirroring those
        // across regions as well
        std::vector<Region> adjacent_regions;
        for (const auto r : regions) {
            if (std::abs(r.col - regions[0].col) == 1 || std::abs(r.row - regions[0].row) == 1) {
                adjacent_regions.push_back(r);
            }
        }
        std::uniform_int_distribution<int> rand_region(0, adjacent_regions.size() - 1);
        std::uniform_int_distribution<int> rand_sign_dist(0, 1);
        auto rand_sign = [&]() -> bool {
            return rand_sign_dist(rng) == 1;
        };

        for (int j = 0; j < MAX_TRIES && map.planets.size() < MAX_PLANETS; j++) {
            const auto base = regions[0];
            const auto region = adjacent_regions[rand_region(rng)];
            const auto midpoint_x = (base.center_x() + region.center_x()) / 2;
            const auto midpoint_y = (base.center_y() + region.center_y()) / 2;

            // Generate two planets around the midpoint
            // (This might generate an extra planet if the # of planets
            // remaining is odd, but oh well)
            const auto r = rand_radius();

            int x1, y1, x2, y2;

            if (region.col != base.col && region.row != base.row) {
                const auto offset = std::min(rand_region_width(), rand_region_height()) / 4;
                // Diagonal relative to each other
                if (rand_sign()) {
                    x1 = midpoint_x - offset;
                    y1 = midpoint_y - offset;
                    x2 = midpoint_x + offset;
                    x2 = midpoint_y + offset;
                }
                else {
                    x1 = midpoint_x - offset;
                    y1 = midpoint_y + offset;
                    x2 = midpoint_x + offset;
                    y2 = midpoint_y - offset;
                }
            }
            else if (region.col != base.col) {
                const auto offset = rand_region_height() / 2;
                // Side-by-side
                x1 = midpoint_x;
                y1 = midpoint_y - offset;
                x2 = midpoint_x;
                y2 = midpoint_y + offset;
            }
            else {
                const auto offset = rand_region_width() / 2;
                // One above the other
                x1 = midpoint_x - offset;
                y1 = midpoint_y;
                x2 = midpoint_x + offset;
                y2 = midpoint_y;
            }

            if (!is_valid_planet_position(x1, y1, r) ||
                !is_valid_planet_position(x2, y2, r)) {
                continue;
            }

            if (std::abs(x1 - x2) < 2 * r + 1 && std::abs(y1 - y2) < 2 * r + 1) {
                continue;
            }

            std::vector<Region> target_regions;
            if (region.col != base.col && region.row != base.row) {
                // Diagonal relative to each other
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col + 1 && r2.row == r1.row + 1) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }
            else if (region.col != base.col) {
                // Side-by-side
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col + 1 && r2.row == r1.row) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }
            else {
                // One above the other
                for (const auto& r1 : regions) {
                    for (const auto& r2 : regions) {
                        if (r2.col == r1.col && r2.row == r1.row + 1) {
                            target_regions.push_back(r1);
                        }
                    }
                }
            }

            for (const auto& target_region : target_regions) {
                map.planets.push_back(hlt::Planet(target_region.x + x1,
                                         target_region.y + y1, r));
                map.planets.push_back(hlt::Planet(target_region.x + x2,
                                         target_region.y + y2, r));
            }
        }

        std::cout << map.map_width << " " << map.map_height << std::endl;
    }
}
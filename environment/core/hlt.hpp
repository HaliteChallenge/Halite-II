#pragma once

#include <list>
#include <vector>
#include <random>
#include <functional>
#include <iostream>
#include <fstream>
#include <assert.h>
#include <array>

extern bool quiet_output;

namespace hlt {
    constexpr auto MAX_PLAYERS = 4;
    constexpr auto MAX_PLAYER_SHIPS = 40;
    constexpr auto MAX_QUEUED_MOVES = 3;

    typedef unsigned char PlayerId;
    typedef size_t        EntityIndex;

    struct Location {
        unsigned short x, y;
    };
    static bool operator==(const Location & l1, const Location & l2) {
        return l1.x == l2.x && l1.y == l2.y;
    }

    struct Entity {
        Location location;
        short    health;

        void kill() {
            health = 0;
        }

        bool is_alive() {
            return health > 0;
        }
    };

    enum DockingStatus {
        Undocked,
        Docking,
        Docked,
    };

    struct Ship : Entity {
        constexpr static auto BASE_HEALTH = 200;

        //! Rotation of the ship, degrees (0-359) from due east
        unsigned short orientation;

        DockingStatus docking_status;
        unsigned short docking_progress;
        EntityIndex docked_planet;
    };

    struct Planet : Entity {
        constexpr static auto DOCK_TURNS = 5;

        PlayerId owner;
        bool owned;

        unsigned short radius;
        unsigned short remaining_production;
        unsigned short docking_spots;

        std::vector<EntityIndex> docked_ships;

        Planet(unsigned short x, unsigned short y, unsigned short radius) {
            location.x = x;
            location.y = y;
            this->radius = radius;
            health = (short) (500 + 100 * sqrt(radius));

            owned = false;
        }
    };

    enum MoveType {
        Rotate,
        Thrust,
        Dock,
    };

    struct Move {
        MoveType type;
        EntityIndex shipId;

        union {
            short rotateBy;
            short thrustBy;
            EntityIndex dockTo;
        } move;
    };

    typedef std::array<std::array<hlt::Move, MAX_PLAYER_SHIPS>, MAX_QUEUED_MOVES> PlayerMoveQueue;
    typedef std::array<PlayerMoveQueue, MAX_PLAYERS> MoveQueue;

    class Map {
    public:
        std::array<std::array<Ship, MAX_PLAYER_SHIPS>, MAX_PLAYERS> ships;
        std::vector<Planet> planets;
        unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

        Map() {
            map_width = 0;
            map_height = 0;
            ships = { {} };
            planets = std::vector<Planet>();
        }

        Map(const Map &otherMap) {
            map_width = otherMap.map_width;
            map_height = otherMap.map_height;
            ships = otherMap.ships;
            planets = otherMap.planets;
        }

        Map(unsigned short width, unsigned short height, unsigned char numberOfPlayers, unsigned int seed) : Map() {
            // TODO: enforce a minimum map size to make sure we always have room for planets

            //Pseudorandom number generator.
            std::mt19937 prg(seed);
            std::uniform_int_distribution<unsigned short> uidw(0, width - 1);
            std::uniform_int_distribution<unsigned short> uidh(0, height - 1);
            std::uniform_int_distribution<unsigned short> uidr(1, std::min(width, height) / 25);
            const auto rand_width = [&]() -> unsigned short { return uidw(prg); };
            const auto rand_height = [&]() -> unsigned short { return uidh(prg); };
            const auto rand_radius = [&]() -> unsigned short { return uidr(prg); };

            //Decides whether to put more players along the horizontal or the vertical.
            bool preferHorizontal = prg() % 2 == 0;

            int dw, dh;
            //Find number closest to square that makes the match symmetric.
            if(preferHorizontal) {
                dh = (int) sqrt(numberOfPlayers);
                while(numberOfPlayers % dh != 0) dh--;
                dw = numberOfPlayers / dh;
            }
            else {
                dw = (int) sqrt(numberOfPlayers);
                while(numberOfPlayers % dw != 0) dw--;
                dh = numberOfPlayers / dw;
            }

            //Figure out chunk width and height accordingly.
            //Matches width and height as closely as it can, but is not guaranteed to match exactly.
            //It is guaranteed to be smaller if not the same size, however.
            int cw = 5 * width / dw;
            int ch = 5 * height / dh;

            map_width = (unsigned short) (cw * dw);
            map_height = (unsigned short) (ch * dh);

            // Divide the map into regions for each player

            class Region {
            public:
                unsigned short width;
                unsigned short height;
                unsigned short x;
                unsigned short y;

                Region(unsigned short _x, unsigned short _y, unsigned short _width, unsigned short _height) {
                    this->x = _x;
                    this->y = _y;
                    this->width = _width;
                    this->height = _height;
                }
            };

            std::vector<Region> regions = std::vector<Region>();
            regions.reserve(numberOfPlayers);

            for (int row = 0; row < dh; row++) {
                for (int col = 0; col < dw; col++) {
                    regions.push_back(Region(col * cw, row * ch, cw, ch));
                }
            }

            // Center the player's starting ships in each region
            for (PlayerId playerId = 0; playerId < numberOfPlayers; playerId++) {
                const auto& region = regions.at(playerId);

                for (int i = 0; i < 3; i++) {
                    ships[playerId][i].health = Ship::BASE_HEALTH;
                    ships[playerId][i].location.x = region.x + (region.width / 2);
                    ships[playerId][i].location.y = region.y + (region.height / 2) - 1 + i;
                }
            }

            // Scatter planets throughout all of space, avoiding the starting ships (centers of regions)
            const auto MAX_PLANETS  = numberOfPlayers * 2;
            const auto MAX_TRIES    = 100;
            const auto MIN_DISTANCE = 5;
            for (int i = 0; i < MAX_PLANETS; i++) {
                for (int j = 0; j < MAX_TRIES; j++) {
                    const auto x = rand_width();
                    const auto y = rand_height();
                    const auto r = rand_radius();

                    for (Region region : regions) {
                        if (getDistance({ region.x, region.y }, { x, y }) < r + MIN_DISTANCE) {
                            goto TRY_AGAIN;
                        }
                    }

                    planets.push_back(Planet(x, y, r));
                    goto NEXT_PLANET;

                    TRY_AGAIN: ;
                }

                NEXT_PLANET: ;
            }

            std::cout << map_width << " " << map_height << std::endl;
        }

        Ship& getShip(PlayerId player, EntityIndex entity) {
            return ships.at(player).at(entity);
        }

        void killShip(Ship& ship) {
            ship.kill();
        }

        float getDistance(Location l1, Location l2) const {
            short dx = l1.x - l2.x;
            short dy = l1.y - l2.y;
            return sqrtf((dx*dx) + (dy*dy));
        }

        float getAngle(Location l1, Location l2) const {
            short dx = l2.x - l1.x;
            short dy = l2.y - l1.y;
            return atan2f(dy, dx);
        }

        //! Damage the given ship, killing it and returning true if the ship health falls below 0
        auto damageShip(Ship &ship, unsigned short damage) -> bool {
            if (ship.health <= damage) {
                killShip(ship);
                return true;
            }
            else {
                ship.health -= damage;
                return false;
            }
        }
    };
}

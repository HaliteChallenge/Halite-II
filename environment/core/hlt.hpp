#pragma once

#include <list>
#include <vector>
#include <random>
#include <functional>
#include <iostream>
#include <fstream>
#include <assert.h>
#include <array>

#define MAX_PLAYERS 4
#define MAX_PLAYER_SHIPS 40

extern bool quiet_output;

namespace hlt {
    typedef unsigned char PlayerId;
    typedef unsigned int  EntityId;

    struct Location {
        unsigned short x, y;
    };
    static bool operator<(const Location & l1, const Location & l2) {
        return ((l1.x + l1.y)*((unsigned int)l1.x + l1.y + 1) / 2) + l1.y < ((l2.x + l2.y)*((unsigned int)l2.x + l2.y + 1) / 2) + l2.y;
    }
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

    struct Ship : Entity {
        //! Rotation of the ship, degrees (0-359) from due east
        unsigned short orientation;
    };

    enum MoveType {
        Rotate,
        Thrust,
        Dock,
    };

    struct Move {
        MoveType type;
        EntityId shipId;

        union {
            short rotateBy;
            short thrustBy;
            EntityId dockTo;
        } move;
    };

    class Map {
    public:
        std::array<std::array<Ship, MAX_PLAYER_SHIPS>, MAX_PLAYERS> ships;
        std::vector<std::vector<bool>> collision;
        unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

        Map() {
            map_width = 0;
            map_height = 0;
            ships = { {} };
            collision = { { } };
        }

        Map(const Map &otherMap) {
            map_width = otherMap.map_width;
            map_height = otherMap.map_height;
            ships = otherMap.ships;
            collision = otherMap.collision;
        }

        Map(unsigned short width, unsigned short height, unsigned char numberOfPlayers, unsigned int seed) : Map() {
            map_width = width;
            map_height = height;

            collision = std::vector<std::vector<bool>>(width, std::vector<bool>(height, false));

            //Pseudorandom number generator.
            std::mt19937 prg(seed);
            std::uniform_real_distribution<double> urd(0.0, 1.0);

            for (PlayerId playerId = 0; playerId < numberOfPlayers; playerId++) {
                ships[playerId][0].health = 200;
                ships[playerId][0].location.x = (unsigned short) playerId;
                collision[playerId][0] = true;
            }

            std::cout << map_width << " " << map_height << std::endl;
        }

        Ship& getShip(PlayerId player, EntityId entity) {
            return ships.at(player).at(entity);
        }

        void killShip(Ship* ship) {
            ship->kill();
            collision[ship->location.x][ship->location.y] = false;
        }

        float getDistance(Location l1, Location l2) const {
            short dx = l1.x - l2.x;
            short dy = l1.y - l2.y;
            return sqrt((dx*dx) + (dy*dy));
        }

        float getAngle(Location l1, Location l2) const {
            short dx = l2.x - l1.x;
            short dy = l2.y - l1.y;
            return atan2(dy, dx);
        }
    };
}

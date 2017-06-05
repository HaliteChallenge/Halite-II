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
    typedef unsigned int  EntityIndex;

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
        unsigned short map_width, map_height; //Number of rows and columns, NOT maximum index.

        Map() {
            map_width = 0;
            map_height = 0;
            ships = { {} };
        }

        Map(const Map &otherMap) {
            map_width = otherMap.map_width;
            map_height = otherMap.map_height;
            ships = otherMap.ships;
        }

        Map(unsigned short width, unsigned short height, unsigned char numberOfPlayers, unsigned int seed) : Map() {
            map_width = width;
            map_height = height;

            //Pseudorandom number generator.
            std::mt19937 prg(seed);
            std::uniform_real_distribution<double> urd(0.0, 1.0);

            for (PlayerId playerId = 0; playerId < numberOfPlayers; playerId++) {
                for (int i = 0; i < 3; i++) {
                    ships[playerId][i].health = 200;
                    ships[playerId][i].location.x = (unsigned short) playerId;
                }
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
            return sqrt((dx*dx) + (dy*dy));
        }

        float getAngle(Location l1, Location l2) const {
            short dx = l2.x - l1.x;
            short dy = l2.y - l1.y;
            return atan2(dy, dx);
        }

        //! Damage the given ship, killing it and returning true if the ship health falls below 0
        auto damageShip(Ship &ship, unsigned short damage) -> bool {
            // TODO: actual damage calculations
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

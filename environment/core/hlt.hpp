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
        constexpr static auto MINIMUM_RADIUS = 3;
        constexpr static auto DOCK_TURNS = 5;

        PlayerId owner;
        bool owned;

        unsigned short radius;
        unsigned short remaining_production;
        unsigned short docking_spots;

        std::vector<EntityIndex> docked_ships;
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
            map_width = width;
            map_height = height;

            //Pseudorandom number generator.
            std::mt19937 prg(seed);
            std::uniform_real_distribution<double> urd(0.0, 1.0);

            for (PlayerId playerId = 0; playerId < numberOfPlayers; playerId++) {
                for (int i = 0; i < 3; i++) {
                    ships[playerId][i].health = Ship::BASE_HEALTH;
                    ships[playerId][i].location.x = (unsigned short) playerId;
                    ships[playerId][i].location.y = i;
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

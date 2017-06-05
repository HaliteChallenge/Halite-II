#ifndef HLT_H
#define HLT_H

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
    typedef size_t EntityIndex;

    struct Location {
        unsigned short x, y;
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.x == l2.x && l1.y == l2.y;
    }

    struct Entity {
        Location location;
        short health;

        void kill() {
            health = 0;
        }

        bool is_alive() const {
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

        auto reset_docking_status() -> void {
            docking_status = DockingStatus::Undocked;
            docking_progress = 0;
            docked_planet = 0;
        }
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
            docking_spots = radius;

            owned = false;
        }
    };

    struct EntityId {
    private:
        int _player_id;
        int _entity_index;
    public:
        EntityId();
        EntityId(PlayerId player, EntityIndex index);

        bool is_valid() const;
        bool is_planet() const;
        bool is_ship() const;

        PlayerId player_id() const;
        EntityIndex entity_index() const;

        static EntityId invalid();
        static EntityId for_planet(EntityIndex index);
    };

    enum MoveType {
        //! Noop is not user-specifiable - instead it's the default command,
        //! used to mean that no command was issued
            Noop = 0,
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

    typedef std::array<std::array<hlt::Move, MAX_PLAYER_SHIPS>,
                       MAX_QUEUED_MOVES> PlayerMoveQueue;
    typedef std::array<PlayerMoveQueue, MAX_PLAYERS> MoveQueue;

    class Map {
    public:
        std::array<std::array<Ship, MAX_PLAYER_SHIPS>, MAX_PLAYERS> ships;
        std::vector<Planet> planets;
        unsigned short map_width,
            map_height; //Number of rows and columns, NOT maximum index.

        Map();
        Map(const Map& otherMap);
        Map(unsigned short width,
            unsigned short height,
            unsigned char numberOfPlayers,
            unsigned int seed);

        auto getShip(PlayerId player, EntityIndex entity) -> Ship&;
        auto getShip(EntityId entity_id) -> Ship&;
        auto getPlanet(EntityId entity_id) -> Planet&;
        auto getEntity(EntityId entity_id) -> Entity&;
        auto getDistance(Location l1, Location l2) const -> float;
        auto getAngle(Location l1, Location l2) const -> float;
        auto killEntity(EntityId& id) -> void;
        //! Damage the given ship, killing it and returning true if the ship health falls below 0
        auto damageEntity(EntityId id, unsigned short damage) -> bool;
    };
}

#endif

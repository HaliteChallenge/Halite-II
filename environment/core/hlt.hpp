#ifndef HLT_H
#define HLT_H

#include <list>
#include <vector>
#include <random>
#include <algorithm>
#include <functional>
#include <iostream>
#include <fstream>
#include <assert.h>
#include <array>
#include <unordered_map>

#include "Constants.hpp"
#include "Entity.hpp"

#include "json.hpp"

extern bool quiet_output;

namespace hlt {
    enum class MoveType {
        //! Noop is not user-specifiable - instead it's the default command,
        //! used to mean that no command was issued
        Noop = 0,
        Thrust,
        Dock,
        Undock,
        //! Error wraps a move that was syntactically valid, but could not be
        //! executed in the current game state.
        Error,
    };

    struct Move {
        MoveType type;
        EntityIndex shipId;

        union {
            struct { unsigned short thrust; unsigned short angle; } thrust;
            EntityIndex dock_to;
        } move;
    };

    template<typename T>
    using entity_map = std::unordered_map<EntityIndex, T>;

    typedef std::array<entity_map<hlt::Move>, MAX_QUEUED_MOVES> PlayerMoveQueue;
    typedef std::array<PlayerMoveQueue, MAX_PLAYERS> MoveQueue;

    class Map {
    public:
        std::array<entity_map<Ship>, MAX_PLAYERS> ships;
        std::vector<Planet> planets;
        unsigned short map_width, map_height;

        Map();
        Map(const Map& other_map);
        Map(unsigned short width, unsigned short height);

        auto get_ship(PlayerId player, EntityIndex entity) -> Ship&;
        auto get_ship(EntityId entity_id) -> Ship&;
        auto get_planet(EntityId entity_id) -> Planet&;
        auto get_entity(EntityId entity_id) -> Entity&;
        auto kill_entity(EntityId entity_id) -> void;
        auto unsafe_kill_entity(EntityId entity_id) -> void;
        auto cleanup_entities() -> void;
        auto get_distance(Location l1, Location l2) const -> double;
        auto get_angle(Location l1, Location l2) const -> double;

        auto location_with_delta(const Location& location, double dx, double dy) -> possibly<Location>;

        auto test(const Location& location, double radius) -> std::vector<EntityId>;
        auto spawn_ship(const Location& location, PlayerId owner) -> EntityIndex;
    };
}

#endif

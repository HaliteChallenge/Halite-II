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
extern bool always_log;

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

    /**
     * Represents a command that may be issued to a ship.
     */
    struct Move {
        MoveType type;
        EntityIndex shipId;

        union {
            struct { unsigned short thrust; unsigned short angle; } thrust;
            EntityIndex dock_to;
        } move;

        auto output_json(hlt::PlayerId player_id, int move_no) const -> nlohmann::json;
    };

    template<typename T>
    using entity_map = std::unordered_map<EntityIndex, T>;

    typedef std::array<entity_map<hlt::Move>, MAX_QUEUED_MOVES> PlayerMoveQueue;
    typedef std::array<PlayerMoveQueue, MAX_PLAYERS> MoveQueue;

    /**
     * Represents the state of the game map during a given turn.
     */
    class Map {
    private:
        /**
         * The index of the next entity to be spawned.
         */
        EntityIndex next_index;

    public:
        /**
         * A map of all the ships in the game, keyed by the player's tag and
         * the ship's index.
         */
        std::array<entity_map<Ship>, MAX_PLAYERS> ships;
        /**
         * A map of all the planets in the game, keyed by the planet's
         * index. Planets which have died are still in this array.
         */
        std::vector<Planet> planets;
        unsigned short map_width, map_height;

        Map();
        Map(const Map& other_map);
        Map(unsigned short width, unsigned short height);

        auto is_valid(EntityId entity_id) -> bool;
        auto within_bounds(const Location& location) const -> bool;
        auto get_ship(PlayerId player, EntityIndex entity) -> Ship&;
        auto get_ship(PlayerId player, EntityIndex entity) const -> const Ship&;
        auto get_ship(EntityId entity_id) -> Ship&;
        auto get_planet(EntityId entity_id) -> Planet&;
        auto get_entity(EntityId entity_id) -> Entity&;
        auto kill_entity(EntityId entity_id) -> void;
        auto unsafe_kill_entity(EntityId entity_id) -> void;
        auto cleanup_entities() -> void;
        auto get_distance(Location l1, Location l2) const -> double;
        /**
         * Create a location with an offset applied, checking if the location
         * is within bounds. If not, the second member of the pair will be
         * false.
         * @param location
         * @param dx
         * @param dy
         * @return
         */
        auto location_with_delta(const Location& location, double dx, double dy) -> possibly<Location>;

        auto test(const Location& location, double radius, double time) -> std::vector<EntityId>;
        auto test_planets(const Location& location, double radius,
                          std::vector<EntityId>& collisions) -> void;
        auto test_ids(const Location& location, double radius,
                      const std::vector<EntityId>& potential,
                      std::vector<EntityId>& result) -> void;
        auto any_collision(const Location& location, double radius,
                           const std::vector<EntityId>& potential) -> bool;
        auto any_planet_collision(const Location& location, double radius) -> bool;
        auto spawn_ship(const Location& location, PlayerId owner) -> EntityIndex;
    };

    struct GameAbort {
        int status;
    };
}

#endif

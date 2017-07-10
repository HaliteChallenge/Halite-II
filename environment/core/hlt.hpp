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

#include "json.hpp"

extern bool quiet_output;

namespace hlt {
    constexpr auto MAX_PLAYERS = 4;
    constexpr auto MAX_PLAYER_SHIPS = 200;
    constexpr auto MAX_QUEUED_MOVES = 1;

    struct GameConstants {
        int PLANETS_PER_PLAYER = 6;
        unsigned int EXTRA_PLANETS = 4;

        int DRAG = 3;
        int MAX_SPEED = 30;
        int MAX_ACCELERATION = 10;

        unsigned short MAX_SHIP_HEALTH = 255;
        unsigned short BASE_SHIP_HEALTH = 255;
        unsigned short DOCKED_SHIP_REGENERATION = 0;

        unsigned int WEAPON_COOLDOWN = 1;
        int WEAPON_RADIUS = 5;
        int WEAPON_DAMAGE = 128;
        unsigned int EXPLOSION_RADIUS = 5;

        unsigned int MAX_DOCKING_DISTANCE = 4;
        unsigned int DOCK_TURNS = 5;
        int PRODUCTION_PER_SHIP = 100;
        unsigned int BASE_PRODUCTIVITY = 25;
        unsigned int ADDITIONAL_PRODUCTIVITY = 15;

        static auto get_mut() -> GameConstants& {
            // Guaranteed initialized only once by C++11
            static GameConstants instance;
            return instance;
        }

        static auto get() -> const GameConstants& {
            return get_mut();
        }

        auto to_json() const -> nlohmann::json;
        auto from_json(const nlohmann::json& json) -> void;
    };

    typedef unsigned char PlayerId;
    typedef unsigned long EntityIndex;

    //! A poor man's std::optional.
    template<typename T>
    using possibly = std::pair<T, bool>;

    struct Location {
        unsigned short pos_x, pos_y;

        friend auto operator<< (std::ostream& ostream, const Location& location) -> std::ostream&;
    };

    struct Velocity {
        short vel_x, vel_y;

        auto accelerate_by(unsigned short magnitude, double angle) -> void;
        auto magnitude() const -> double;
        auto angle() const -> double;
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.pos_x == l2.pos_x && l1.pos_y == l2.pos_y;
    }

    struct Entity {
        Location location;
        unsigned short health;
        //! The radius of the entity, in terms of grid cells from the center.
        //! Ships have a radius of 0 (they occupy only the center). A planet
        //! with radius=1 will occupy 5 grid cells (the center and the four
        //! adjacent cells in the cardinal directions)
        unsigned short radius;

        void kill() {
            health = 0;
        }

        bool is_alive() const {
            return health > 0;
        }

        auto heal(unsigned short points) -> void {
            health = std::min(GameConstants::get().MAX_SHIP_HEALTH,
                              static_cast<unsigned short>(health + points));
        }
    };

    enum class DockingStatus {
        Undocked = 0,
        Docking = 1,
        Docked = 2,
        Undocking = 3,
    };

    struct Ship : Entity {
        Velocity velocity;

        unsigned int weapon_cooldown;

        DockingStatus docking_status;
        unsigned int docking_progress;
        EntityIndex docked_planet;

        auto reset_docking_status() -> void {
            docking_status = DockingStatus::Undocked;
            docking_progress = 0;
            docked_planet = 0;
        }

        auto revive(const Location& loc) -> void {
            health = GameConstants::get().BASE_SHIP_HEALTH;
            location = loc;
            weapon_cooldown = 0;
            radius = 0;
            velocity = { 0, 0 };
            docking_status = DockingStatus::Undocked;
            docking_progress = 0;
            docked_planet = 0;
        }
    };

    struct Planet : Entity {
        PlayerId owner;
        bool owned;

        unsigned short remaining_production;
        unsigned short current_production;
        unsigned short docking_spots;

        //! Contains IDs of all ships in the process of docking or undocking,
        //! as well as docked ships.
        std::vector<EntityIndex> docked_ships;

        Planet(unsigned short x, unsigned short y, unsigned short radius) {
            location.pos_x = x;
            location.pos_y = y;
            this->radius = radius;
            docking_spots = radius;
            remaining_production = static_cast<unsigned short>(std::sqrt(10 * radius)) * 100;
            current_production = 0;
            health = static_cast<unsigned short>(remaining_production * GameConstants::get().MAX_SHIP_HEALTH / 100);
            docked_ships = std::vector<EntityIndex>();

            owned = false;
        }

        auto add_ship(EntityIndex ship) -> void;
        auto remove_ship(EntityIndex ship) -> void;
    };

    enum class EntityType {
        InvalidEntity,
        ShipEntity,
        PlanetEntity,
    };

    //! A way to uniquely identify an Entity, regardless of its type.
    struct EntityId {
    private:
        //! Planets are unowned, and have player ID == -1.
        int _player_id;
        int _entity_index;
        EntityId();

    public:
        EntityType type;

        auto is_valid() const -> bool;

        auto player_id() const -> PlayerId;
        auto entity_index() const -> EntityIndex;

        //! Construct an entity ID representing an invalid entity.
        static auto invalid() -> EntityId;
        //! Construct an entity ID for the given planet.
        static auto for_planet(EntityIndex index) -> EntityId;
        //! Construct an entity ID for the given ship.
        static auto for_ship(PlayerId player_id, EntityIndex index) -> EntityId;

        friend auto operator<< (std::ostream& ostream, const EntityId& id) -> std::ostream&;
        friend auto operator== (const EntityId& id1, const EntityId& id2) -> bool;
        friend auto operator!= (const EntityId& id1, const EntityId& id2) -> bool;
    };

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
        Map(const Map& other_map);
        Map(unsigned short width, unsigned short height);

        auto get_ship(PlayerId player, EntityIndex entity) -> Ship&;
        auto get_ship(EntityId entity_id) -> Ship&;
        auto get_planet(EntityId entity_id) -> Planet&;
        auto get_entity(EntityId entity_id) -> Entity&;
        auto get_distance(Location l1, Location l2) const -> float;
        auto get_angle(Location l1, Location l2) const -> float;

        auto location_with_delta(const Location& location, int dx, int dy) -> possibly<Location>;
    };
}

#endif

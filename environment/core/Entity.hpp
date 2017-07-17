//
// Created by David Li on 7/17/17.
//

#ifndef ENVIRONMENT_ENTITY_H
#define ENVIRONMENT_ENTITY_H

#include <cmath>
#include <iostream>
#include <utility>
#include <vector>

#include "Constants.hpp"

namespace hlt {
    typedef unsigned char PlayerId;
    typedef unsigned long EntityIndex;

    //! A poor man's std::optional.
    template<typename T>
    using possibly = std::pair<T, bool>;

    struct Location {
        double pos_x, pos_y;

        auto distance(const Location& other) const -> double;
        auto distance2(const Location& other) const -> double;

        friend auto operator<< (std::ostream& ostream, const Location& location) -> std::ostream&;
    };

    struct Velocity {
        double vel_x, vel_y;

        auto accelerate_by(double magnitude, double angle) -> void;
        auto magnitude() const -> double;
        auto angle() const -> double;
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.pos_x == l2.pos_x && l1.pos_y == l2.pos_y;
    }

    struct Entity {
        Location location;
        unsigned short health;
        //! The radius of the entity, in terms of units.
        double radius;

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
            radius = 1;
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
            health = static_cast<unsigned short>(radius * GameConstants::get().MAX_SHIP_HEALTH);
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

        friend class std::hash<EntityId>;
    };
}

namespace std {
    template<> class hash<hlt::EntityId> {
    public:
        auto operator()(const hlt::EntityId& id) const -> size_t {
            return static_cast<size_t>(
                (id._entity_index << 8) |
                    (id._player_id & 0xFF));
        }
    };
};

#endif //ENVIRONMENT_ENTITY_H

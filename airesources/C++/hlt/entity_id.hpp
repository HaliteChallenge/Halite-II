#pragma once

#include <ostream>
#include "types.hpp"

namespace hlt {
    /**
     * The type of an entity represented by an entity ID.
     */
    enum class EntityType {
        //! This entity ID does not represent an actual entity.
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

        friend struct std::hash<EntityId>;
    };
}

namespace std {
    template<> struct hash<hlt::EntityId> {
    public:
        size_t operator()(const hlt::EntityId& id) const {
            return static_cast<size_t>(
                    (id._entity_index << 8) | (id._player_id & 0xFF));
        }
    };
}

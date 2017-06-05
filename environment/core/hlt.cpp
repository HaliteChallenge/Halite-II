//
// Created by David Li on 6/5/17.
//

#ifndef HLT_H
#define HLT_H

#include "hlt.hpp"

namespace hlt {
    EntityId::EntityId() {
        _player_id = -1;
        _entity_index = -1;
    }

    EntityId::EntityId(PlayerId player, EntityIndex index) {
        _player_id = player;
        _entity_index = index;
    }

    bool EntityId::is_valid() const {
        return _player_id >= -1 && _entity_index >= 0;
    }

    bool EntityId::is_planet() const {
        return is_valid() && _player_id == -1;
    }

    bool EntityId::is_ship() const {
        return is_valid() && _player_id >= 0;
    }

    EntityId EntityId::invalid() {
        return EntityId();
    }

    PlayerId EntityId::player_id() const {
        return static_cast<PlayerId>(_player_id);
    }

    EntityIndex EntityId::entity_index() const {
        return static_cast<EntityIndex>(_entity_index);
    }
}

#endif
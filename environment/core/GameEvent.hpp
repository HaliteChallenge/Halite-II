//
// Created by David Li on 7/18/17.
//

#ifndef ENVIRONMENT_GAMEEVENT_HPP
#define ENVIRONMENT_GAMEEVENT_HPP

#include "Entity.hpp"
#include "hlt.hpp"

#include "json.hpp"

/**
 * An event that happens during game simulation. Recorded for the replay, so
 * that visualizers have more information to use.
 */
struct Event {
    virtual auto serialize() -> nlohmann::json = 0;

    Event() {};
};

struct DestroyedEvent : Event {
    hlt::EntityId id;
    hlt::Location location;
    double time;
    double radius;

    DestroyedEvent(hlt::EntityId id_, hlt::Location location_,
                   double radius_, double time_)
        : id(id_), location(location_), time(time_), radius(radius_) {};

    auto serialize() -> nlohmann::json override {
        return nlohmann::json{
            { "event", "destroyed" },
            { "entity", id },
            { "x", location.pos_x },
            { "y", location.pos_y },
            { "radius", radius },
            { "time", time },
        };
    }
};

struct AttackEvent : Event {
    hlt::EntityId id;
    hlt::Location location;
    double time;

    std::vector<hlt::EntityId> targets;
    std::vector<hlt::Location> target_locations;

    AttackEvent(hlt::EntityId id_, hlt::Location location_, double time_,
                std::vector<hlt::EntityId> targets_,
                std::vector<hlt::Location> target_locations_) :
        id(id_), location(location_), time(time_), targets(targets_),
        target_locations(target_locations_) {};

    auto serialize() -> nlohmann::json override {
        std::vector<nlohmann::json> target_locations;
        target_locations.reserve(targets.size());
        for (auto& location : targets) {
            target_locations.push_back(location);
        }
        return nlohmann::json{
            { "event", "attack" },
            { "entity", id },
            { "x", location.pos_x },
            { "y", location.pos_y },
            { "targets", targets },
            { "target_locations", target_locations },
            { "time", time },
        };
    }
};

/**
 * Ships that simultaneously dock to a planet attack each other.
 */
struct ContentionAttackEvent : Event {
    hlt::EntityId planet;
    hlt::Location planet_location;

    std::vector<hlt::EntityId> participants;
    std::vector<hlt::Location> participant_locations;

    ContentionAttackEvent(
        hlt::EntityId planet, hlt::Location planet_location,
        std::vector<hlt::EntityId> participants,
        std::vector<hlt::Location> participant_locations
    ) : planet(planet), planet_location(planet_location),
        participants(participants),
        participant_locations(participant_locations) {};

    auto serialize() -> nlohmann::json override {
        return nlohmann::json{
            { "event", "contention" },
            { "entity", planet },
            { "x", planet_location.pos_x },
            { "y", planet_location.pos_y },
            { "participants", participants },
            { "participant_locations", participant_locations },
        };
    }
};

struct SpawnEvent : Event {
    hlt::EntityId id;
    hlt::EntityId planet;
    hlt::Location location;
    hlt::Location planet_location;

    SpawnEvent(hlt::EntityId id_, hlt::EntityId planet_,
               hlt::Location location_, hlt::Location planet_location_)
        : id(id_), planet(planet_),
          location(location_), planet_location(planet_location_) {}

    auto serialize() -> nlohmann::json override {
        return nlohmann::json{
            { "event", "spawned" },
            { "entity", id },
            { "planet", planet },
            { "x", location.pos_x },
            { "y", location.pos_y },
            { "planet_x", planet_location.pos_x },
            { "planet_y", planet_location.pos_y },
        };
    }
};

#endif //ENVIRONMENT_GAMEEVENT_HPP

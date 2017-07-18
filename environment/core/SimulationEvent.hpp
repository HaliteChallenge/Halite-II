//
// Created by David Li on 7/18/17.
//

#ifndef ENVIRONMENT_SIMULATIONEVENT_HPP
#define ENVIRONMENT_SIMULATIONEVENT_HPP

#include <cassert>
#include <iostream>
#include <unordered_set>

#include "Entity.hpp"

/**
 * How to round event times for the purpose of attack and collision resolution.
 */
constexpr auto EVENT_TIME_PRECISION = 10000;

// TODO: document
enum class SimulationEventType {
    Attack,
    Collision,
};

auto operator<<(std::ostream& os, const SimulationEventType& ty) -> std::ostream&;

struct SimulationEvent {
    SimulationEventType type;
    hlt::EntityId id1;
    hlt::EntityId id2;
    double time;

    auto operator==(const SimulationEvent &rhs) const -> bool {
        return type == rhs.type &&
            ((id1 == rhs.id1 && id2 == rhs.id2) ||
                (id1 == rhs.id2 && id2 == rhs.id1));
    }

    auto operator!=(const SimulationEvent &rhs) const -> bool {
        return !(rhs == *this);
    }

    friend auto operator<<(std::ostream &os, const SimulationEvent &event) -> std::ostream& {
        os << "SimulationEvent(type: " << event.type
           << " id1: " << event.id1 << " id2: " << event.id2
           << " time: " << event.time << ")";
        return os;
    }
};

namespace std {
    template<> struct hash<SimulationEvent> {
        auto operator()(const SimulationEvent& ev) const -> std::size_t {
            // TODO: this is a TERRIBLE hash function
            return std::hash<hlt::EntityId>{}(ev.id1) ^
                std::hash<hlt::EntityId>{}(ev.id2) ^
                static_cast<size_t>(ev.type);
        }
    };
}

auto collision_time(
    double r,
    const hlt::Location& loc1, const hlt::Location& loc2,
    const hlt::Velocity& vel1, const hlt::Velocity& vel2
) -> std::pair<bool, double>;
auto collision_time(double r, const hlt::Ship& ship1, const hlt::Ship& ship2) -> std::pair<bool, double>;
auto collision_time(double r, const hlt::Ship& ship1, const hlt::Planet& ship2) -> std::pair<bool, double>;
auto might_attack(double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool;
auto might_collide(double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool;
auto round_event_time(double t) -> double;

auto find_events(
    std::unordered_set<SimulationEvent>& unsorted_events,
    const hlt::PlayerId player1, const hlt::PlayerId& player2,
    const hlt::EntityId id1, const hlt::EntityId& id2,
    const hlt::Ship& ship1, const hlt::Ship& ship2) -> void;

#endif //ENVIRONMENT_SIMULATIONEVENT_HPP

#ifndef ENVIRONMENT_SIMULATIONEVENT_HPP
#define ENVIRONMENT_SIMULATIONEVENT_HPP

#include <cassert>
#include <functional>
#include <iostream>
#include <unordered_set>
#include <vector>

#include "Entity.hpp"
#include "hlt.hpp"

/**
 * How to round event times for the purpose of attack and collision resolution.
 */
constexpr auto EVENT_TIME_PRECISION = 10000;

enum class SimulationEventType {
    Attack,
    Collision,
    //! A ship tries to fly off the map boundary.
    Desertion,
};

auto operator<<(std::ostream& os, const SimulationEventType& ty) -> std::ostream&;

/**
 * This structure is INVALID as soon as the underlying game map is
 * mutated.
 */
struct CollisionMap {
    constexpr static auto CELL_SIZE = 32;

    std::vector<std::vector<std::vector<hlt::EntityId>>> cells;

    int width, height;

    CollisionMap(const hlt::Map& game_map,
                 const std::function<double(const hlt::Ship&)> radius_func);

    auto rebuild(const hlt::Map& game_map,
                 const std::function<double(const hlt::Ship&)> radius_func) -> void;
    auto test(const hlt::Location& location, double radius,
              std::vector<hlt::EntityId>& potential_collisions) -> void;
    auto add(const hlt::Location& location, double radius,
             hlt::EntityId id) -> void;
};

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
auto collision_time(long double r, const hlt::Ship& ship1, const hlt::Ship& ship2) -> std::pair<bool, long double>;
auto collision_time(long double r, const hlt::Ship& ship1, const hlt::Planet& ship2) -> std::pair<bool, long double>;
auto might_attack(long double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool;
auto might_collide(long double distance, const hlt::Ship& ship1, const hlt::Ship& ship2) -> bool;
auto round_event_time(double t) -> double;

auto find_events(
    std::unordered_set<SimulationEvent>& unsorted_events,
    const hlt::EntityId id1, const hlt::EntityId& id2,
    const hlt::Ship& ship1, const hlt::Ship& ship2) -> void;

#endif //ENVIRONMENT_SIMULATIONEVENT_HPP

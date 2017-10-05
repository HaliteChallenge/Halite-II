#include "AsteroidCluster.hpp"

#include <functional>

#ifdef _WIN32
#define _USE_MATH_DEFINES
#include <cmath>
#endif

namespace mapgen {
    AsteroidCluster::AsteroidCluster(unsigned int _seed)
        : Generator(_seed) {}

    auto AsteroidCluster::generate(
        hlt::Map& map,
        unsigned int num_players,
        unsigned int effective_players) -> std::vector<PointOfInterest> {

        auto extra_planets = hlt::GameConstants::get().EXTRA_PLANETS;
        const auto center_x = map.map_width / 2.0;
        const auto center_y = map.map_height / 2.0;

        const auto min_separation =
            std::sqrt(std::min(map.map_width, map.map_height)) / 0.5;
        auto rand_angle = std::bind(
            std::uniform_real_distribution<double>(0, 2 * M_PI), std::ref(rng));

        auto spawn_zones = std::vector<Zone>();

        auto spawn_angle_offset = rand_angle();
        const auto spawn_angle_step = 2 * M_PI / effective_players;
        const auto spawn_radius = std::min(map.map_width, map.map_height) / 2.0;
        for (int player_idx = 0; player_idx < effective_players;
             player_idx++) {
            spawn_zones.emplace_back(
                hlt::Location{
                    center_x + spawn_radius * std::cos(spawn_angle_offset),
                    center_y + spawn_radius * std::sin(spawn_angle_offset),
                },
                2);
            spawn_angle_offset += spawn_angle_step;
        }

        // Temporary storage for the planets created in a particular orbit
        auto planets = std::vector<Zone>();

        auto is_ok_location = [&](const hlt::Location& location, double radius) -> bool {
            if (location.pos_x - radius < 0 || location.pos_x + radius > map.map_width ||
                location.pos_y - radius < 0 || location.pos_y + radius > map.map_height) {
                return false;
            }

            for (const auto& zone : spawn_zones) {
                const auto min_distance = zone.radius + radius + min_separation;
                if (map.get_distance(zone.location, location)
                    <= min_distance) {
                    return false;
                }
            }

            for (const auto& zone : planets) {
                const auto min_distance = zone.radius + radius + 5;
                if (map.get_distance(zone.location, location)
                    <= min_distance) {
                    return false;
                }
            }

            for (const auto& planet : map.planets) {
                const auto min_distance = planet.radius + radius + min_separation;
                if (map.get_distance(planet.location, location)
                    <= min_distance) {
                    return false;
                }
            }
            return true;
        };

        // Planet in center
        if (extra_planets > 0) {
            extra_planets--;
            const auto big_radius =
                std::sqrt(std::min(map.map_width, map.map_height));
            const auto small_radius =
                std::sqrt(std::min(map.map_width, map.map_height) / 1.5);

            for (auto attempt = 0; attempt < 100; attempt++) {
                const auto location = hlt::Location{center_x, center_y};
                const auto radius =
                    std::uniform_real_distribution<>(small_radius, big_radius)(
                        rng);
                if (is_ok_location(location, radius)) {
                    map.planets.emplace_back(
                        location.pos_x,
                        location.pos_y,
                        radius
                    );
                    break;
                }
            }
        }

        for (hlt::PlayerId player_id = 0; player_id < num_players;
             player_id++) {
            const auto& zone = spawn_zones[player_id];
            for (int i = 0; i < 3; i++) {
                // Spread out ships to make it less likely they'll collide
                // in the start
                map.spawn_ship(hlt::Location{
                    zone.location.pos_x,
                    zone.location.pos_y - 2 * (i - 1),
                }, player_id);
            }
        }

        return {};
    }

    auto AsteroidCluster::name() -> std::string {
        return "AsteroidCluster";
    }
}

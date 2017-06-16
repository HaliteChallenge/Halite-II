//
// Created by David Li on 6/15/17.
//

#include "SolarSystem.h"

#include <functional>

namespace mapgen {
    SolarSystem::SolarSystem(unsigned int _seed)
        : Generator(_seed) {}

    struct Zone {
        hlt::Location location;
        unsigned short radius;

        Zone(hlt::Location _location, unsigned short _radius)
            : location(_location), radius(_radius) {}
    };

    auto SolarSystem::generate(
        hlt::Map& map,
        unsigned int num_players,
        unsigned int effective_players) -> std::vector<PointOfInterest> {
        assert(effective_players == 2 || effective_players == 4);
        assert(num_players <= effective_players);

        auto spawn_zones = std::vector<Zone>();

        if (effective_players == 2) {
            auto prefer_horizontal = rng() % 2 == 0;

            if (prefer_horizontal) {
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<unsigned short>(map.map_width / 4),
                        static_cast<unsigned short>(map.map_height / 2) },
                    1);
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<unsigned short>(3 * map.map_width / 4),
                        static_cast<unsigned short>(map.map_height / 2) },
                    1);
            } else {
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<unsigned short>(map.map_width / 2),
                        static_cast<unsigned short>(map.map_height / 4) },
                    1);
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<unsigned short>(map.map_width / 2),
                        static_cast<unsigned short>(3 * map.map_height / 4) },
                    1);
            }
        } else {
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<unsigned short>(map.map_width / 4),
                    static_cast<unsigned short>(map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<unsigned short>(3 * map.map_width / 4),
                    static_cast<unsigned short>(map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<unsigned short>(map.map_width / 4),
                    static_cast<unsigned short>(3 * map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<unsigned short>(3 * map.map_width / 4),
                    static_cast<unsigned short>(3 * map.map_height / 4) },
                1);
        }

        const auto planets_per_player =
            hlt::GameConstants::get().PLANETS_PER_PLAYER;
        const auto total_planets = effective_players * planets_per_player;
        auto extra_planets = hlt::GameConstants::get().EXTRA_PLANETS;
        const auto center_x = map.map_width / 2;
        const auto center_y = map.map_height / 2;

        const auto max_radius = static_cast<int>(
            std::sqrt(std::min(map.map_width, map.map_height)) / 2);
        auto rand_x_axis = std::bind(
            std::uniform_int_distribution<int>(1, map.map_width / 2 - 1), std::ref(rng));
        auto rand_y_axis = std::bind(
            std::uniform_int_distribution<int>(1, map.map_height / 2 - 1), std::ref(rng));
        auto rand_angle = std::bind(
            std::uniform_real_distribution<double>(0, 2 * M_PI), std::ref(rng));
        auto rand_radius =
            std::bind(std::uniform_int_distribution<int>(3, max_radius), std::ref(rng));
        auto rand_planets_generated =
            std::bind(std::uniform_int_distribution<>(1, std::max(2, planets_per_player / 2)), std::ref(rng));

        // Temporary storage for the planets created in a particular orbit
        auto planets = std::vector<Zone>();

        auto is_ok_location = [&](const hlt::Location& location, int radius) -> bool {
            // I promise this pun was an accident
            for (const auto& zone : spawn_zones) {
                const auto min_distance = zone.radius + radius + 15;
                if (map.get_distance(zone.location, location)
                    <= min_distance) {
                    return false;
                }
            }

            for (const auto& zone : planets) {
                const auto min_distance = zone.radius + radius + 3;
                if (map.get_distance(zone.location, location)
                    <= min_distance) {
                    return false;
                }
            }

            for (const auto& planet : map.planets) {
                const auto min_distance = planet.radius + radius + 15;
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
            const auto big_radius = static_cast<int>(
                std::sqrt(std::min(map.map_width, map.map_height)));
            const auto small_radius = static_cast<int>(
                std::sqrt(std::min(map.map_width, map.map_height) / 1.5));

            for (auto attempt = 0; attempt < 100; attempt++) {
                const auto location = hlt::Location{
                    static_cast<unsigned short>(center_x),
                    static_cast<unsigned short>(center_y),
                };
                const auto radius =
                    std::uniform_int_distribution<>(small_radius, big_radius)(
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

        // Store the orbits of the planets we generate, so the visualizer
        // can use them
        auto orbits = std::vector<PointOfInterest>();

        auto total_attempts = 0;
        while (map.planets.size() < total_planets && total_attempts < 10000) {
            // Planets to generate per player this iteration
            auto planets_to_generate = rand_planets_generated() * 2;
            if (map.planets.size() + planets_to_generate > total_planets) {
                planets_to_generate = 2;
            }

            for (auto attempt = 0; attempt < 100; attempt++) {
                planets.clear();
                total_attempts++;

                const auto ellipse_x_axis = rand_x_axis();
                const auto ellipse_y_axis = rand_y_axis();
                const auto offset = rand_angle();
                const auto step = 2.0 * M_PI / planets_to_generate;
                const auto radius = rand_radius();

                for (auto planet_index = 0;
                     planet_index < planets_to_generate;
                     planet_index++) {
                    const auto angle = offset + planet_index * step;
                    const auto x = static_cast<unsigned short>(
                        center_x + ellipse_x_axis * std::cos(angle));
                    const auto y = static_cast<unsigned short>(
                        center_y + ellipse_y_axis * std::sin(angle));
                    const auto location = hlt::Location{ x, y };

                    if (!is_ok_location(location, radius)) {
                        goto TRY_AGAIN;
                    }

                    planets.emplace_back(location, radius);
                }

                for (const auto& zone : planets) {
                    map.planets.emplace_back(zone.location.pos_x,
                                             zone.location.pos_y,
                                             zone.radius);
                }

                orbits.push_back({ hlt::Location{
                    static_cast<unsigned short>(center_x),
                    static_cast<unsigned short>(center_y),
                }, ellipse_x_axis, ellipse_y_axis });

                break;

                TRY_AGAIN:;
            }
        }

        if (extra_planets > 0) {
            // TODO: can we make this composable?
            const auto choice = std::uniform_int_distribution<>(0, 0)(rng);
            if (choice == 0) {
            }
            else if (choice == 1) {
                // Line of planets down vertical axis

            }
            else if (choice == 2) {
                // Line of planets down horizontal axis

            }
            else if (choice == 4) {
                // Planets in corners
            }
        }

        for (hlt::PlayerId player_id = 0; player_id < num_players;
             player_id++) {
            const auto& zone = spawn_zones[player_id];
            for (int i = 0; i < 3; i++) {
                // Spread out ships to make it less likely they'll collide
                // in the start
                map.ships[player_id][i].revive(hlt::Location{
                    zone.location.pos_x,
                    static_cast<unsigned short>(zone.location.pos_y - 2 * (i - 1)),
                });
            }
        }

        return orbits;
    }
}

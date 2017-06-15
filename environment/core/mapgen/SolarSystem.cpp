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

    auto SolarSystem::generate(hlt::Map& map,
                               unsigned int num_players,
                               unsigned int effective_players) -> void {
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

        const auto max_radius = static_cast<int>(
            std::sqrt(std::min(map.map_width, map.map_height)) / 2);
        auto rand_x_axis = std::bind(
            std::uniform_int_distribution<int>(1, map.map_width / 2 - 1), rng);
        auto rand_y_axis = std::bind(
            std::uniform_int_distribution<int>(1, map.map_height / 2 - 1), rng);
        auto rand_angle = std::bind(
            std::uniform_real_distribution<double>(0, 2 * M_PI), rng);
        auto rand_radius =
            std::bind(std::uniform_int_distribution<int>(2, max_radius), rng);
        auto rand_planets_generated =
            std::bind(std::discrete_distribution<int>({ 0, 70, 30 }), rng);

        const auto center_x = map.map_width / 2;
        const auto center_y = map.map_height / 2;

        const auto planets_per_player =
            hlt::GameConstants::get().PLANETS_PER_PLAYER;
        const auto total_planets = num_players * planets_per_player;

        // Temporary storage for the planets created in a particular orbit
        auto planets = std::vector<Zone>();

        while (map.planets.size() < total_planets) {
            // Planets to generate per player this iteration
            // We want a chance to double up on planets in an orbit to keep it
            // interesting, but we should be careful not to make too many planets
            auto planets_to_generate =
                rand_planets_generated() * planets_per_player;

            if (planets_to_generate + map.planets.size() > total_planets) {
                planets_to_generate = 1 * planets_per_player;
            }

            for (auto attempt = 0; attempt < 500; attempt++) {
                planets.clear();

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

                    // I promise this pun was an accident
                    for (const auto& zone : spawn_zones) {
                        const auto min_distance = zone.radius + radius + 5;
                        if (map.get_distance(zone.location, location)
                            <= min_distance) {
                            goto TRY_AGAIN;
                        }
                    }

                    for (const auto& zone : planets) {
                        const auto min_distance = zone.radius + radius + 5;
                        if (map.get_distance(zone.location, location)
                            <= min_distance) {
                            goto TRY_AGAIN;
                        }
                    }

                    for (const auto& planet : map.planets) {
                        const auto min_distance = planet.radius + radius + 5;
                        if (map.get_distance(planet.location, location)
                            <= min_distance) {
                            goto TRY_AGAIN;
                        }
                    }

                    planets.emplace_back(location, radius);
                }

                for (const auto& zone : planets) {
                    map.planets.emplace_back(zone.location.pos_x,
                                             zone.location.pos_y,
                                             zone.radius);
                }
                break;

                TRY_AGAIN:;
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
    }
}

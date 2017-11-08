#include "SolarSystem.hpp"

#include <functional>

#ifdef _WIN32
#define _USE_MATH_DEFINES
#include <cmath>
#endif

namespace mapgen {
    SolarSystem::SolarSystem(unsigned int _seed)
        : Generator(_seed) {}

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
                        static_cast<double>(map.map_width / 4),
                        static_cast<double>(map.map_height / 2) },
                    1);
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<double>(3 * map.map_width / 4),
                        static_cast<double>(map.map_height / 2) },
                    1);
            } else {
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<double>(map.map_width / 2),
                        static_cast<double>(map.map_height / 4) },
                    1);
                spawn_zones.emplace_back(
                    hlt::Location{
                        static_cast<double>(map.map_width / 2),
                        static_cast<double>(3 * map.map_height / 4) },
                    1);
            }
        } else {
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<double>(map.map_width / 4),
                    static_cast<double>(map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<double>(3 * map.map_width / 4),
                    static_cast<double>(map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<double>(map.map_width / 4),
                    static_cast<double>(3 * map.map_height / 4) },
                1);
            spawn_zones.emplace_back(
                hlt::Location{
                    static_cast<double>(3 * map.map_width / 4),
                    static_cast<double>(3 * map.map_height / 4) },
                1);
        }

        const auto planets_per_player =
            hlt::GameConstants::get().PLANETS_PER_PLAYER;
        const auto total_planets = effective_players * planets_per_player;
        auto extra_planets = hlt::GameConstants::get().EXTRA_PLANETS;
        const auto center_x = map.map_width / 2.0;
        const auto center_y = map.map_height / 2.0;

        const auto min_radius =
            std::max(4.0, std::sqrt(std::min(map.map_width, map.map_height)) / 4);
        const auto max_radius =
            std::max(5.0, std::sqrt(std::min(map.map_width, map.map_height)));
        const auto min_separation =
            std::sqrt(std::min(map.map_width, map.map_height)) / 1.5;
        auto rand_x_axis = std::bind(
            util::uniform_int_distribution<int>(1, map.map_width / 2 - 1), std::ref(rng));
        auto rand_y_axis = std::bind(
            util::uniform_int_distribution<int>(1, map.map_height / 2 - 1), std::ref(rng));
        auto rand_angle = std::bind(
            util::uniform_real_distribution<double>(0, 2 * M_PI), std::ref(rng));
        auto rand_radius =
            std::bind(util::uniform_real_distribution<double>(min_radius, max_radius), std::ref(rng));
        auto rand_planets_generated =
            std::bind(util::uniform_int_distribution<>(2, std::max(2, planets_per_player / 2)), std::ref(rng));

        // Temporary storage for the planets created in a particular orbit
        auto planets = std::vector<Zone>();

        auto is_ok_location = [&](const hlt::Location& location, double radius) -> bool {
            // Make sure the entirety of the docking area is within map bounds
            radius += hlt::GameConstants::get().DOCK_RADIUS;
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

        if (extra_planets > 0) {
            const auto big_radius =
                std::max(4.0, std::sqrt(std::min(map.map_width, map.map_height)) / 2);
            const auto small_radius =
                std::max(2.0, std::sqrt(std::min(map.map_width, map.map_height)) / 3);
            // Stick one planet in the center
            if (extra_planets == 1) {
                map.planets.emplace_back(center_x, center_y, big_radius * 1.5);
            }
            // Generate a cluster of small planets in the center
            // Cluster helps make sure trivial bots don't get stuck from all
            // docking to a single center planet at the same time
            else if (extra_planets > 1) {
                const auto radius =
                    util::uniform_real_distribution<>(small_radius, big_radius)(rng);
                const auto distance_from_center = 2 * radius +
                    2 * hlt::GameConstants::get().SHIP_RADIUS;

                for (auto i = 0; i < 4; i++) {
                    const auto angle = i * M_PI / 2 + (M_PI / 4);
                    const auto location = hlt::Location{
                        center_x + distance_from_center * std::cos(angle),
                        center_y + distance_from_center * std::sin(angle),
                    };

                    map.planets.emplace_back(location.pos_x, location.pos_y, radius);
                }
            }
        }

        // Store the orbits of the planets we generate, so the visualizer
        // can use them
        auto orbits = std::vector<PointOfInterest>();

        auto total_attempts = 0;
        while (map.planets.size() < total_planets && total_attempts < MAX_TOTAL_ATTEMPTS) {
            // Planets to generate per player this iteration
            auto planets_to_generate = rand_planets_generated() * 2;
            if (map.planets.size() + planets_to_generate > total_planets) {
                planets_to_generate = 4;
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
                    const auto x = center_x + ellipse_x_axis * std::cos(angle);
                    const auto y = center_y + ellipse_y_axis * std::sin(angle);
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

                orbits.push_back({ hlt::Location{center_x, center_y,},
                                   ellipse_x_axis, ellipse_y_axis });

                break;

                TRY_AGAIN:;
            }
        }

        while (extra_planets > 0 && total_attempts < MAX_TOTAL_ATTEMPTS) {
            total_attempts++;

            const auto choice = util::uniform_int_distribution<>(1, 2)(rng);
            if (choice == 1) {
                // Line of planets down vertical axis
                auto offset = util::uniform_real_distribution<>(max_radius, map.map_height / 2 - max_radius)(rng);
                auto radius = rand_radius();
                auto location1 = hlt::Location{center_x, center_y + offset};
                auto location2 = hlt::Location{center_x, center_y - offset};
                if (is_ok_location(location1, radius) &&
                    is_ok_location(location2, radius)) {
                    // Avoid underflow
                    if (extra_planets <= 2) extra_planets = 0;
                    else extra_planets -= 2;

                    planets.emplace_back(location1, radius);
                    planets.emplace_back(location2, radius);
                }
            }
            else if (choice == 2) {
                // Line of planets down horizontal axis
                auto offset = util::uniform_real_distribution<>(max_radius, map.map_width / 2 - max_radius)(rng);
                auto radius = rand_radius();
                auto location1 = hlt::Location{center_x + offset, center_y};
                auto location2 = hlt::Location{center_x - offset, center_y};

                if (is_ok_location(location1, radius) &&
                    is_ok_location(location2, radius)) {
                    // Avoid underflow
                    if (extra_planets <= 2) extra_planets = 0;
                    else extra_planets -= 2;

                    planets.emplace_back(location1, radius);
                    planets.emplace_back(location2, radius);
                }
            }
            else if (choice == 3) {
                // Planets in corners
            }
        }

        // Generate clusters of small planets in the corners (disabled right now)
        if (extra_planets > 12) {
            const auto big_radius =
                std::max(3.0, std::sqrt(std::min(map.map_width, map.map_height)) / 4);
            const auto small_radius =
                std::max(2.0, std::sqrt(std::min(map.map_width, map.map_height) / 5));
            const auto radius =
                util::uniform_real_distribution<>(small_radius, big_radius)(rng);
            const auto distance_from_center = 2 * radius +
                2 * hlt::GameConstants::get().SHIP_RADIUS;

            std::vector<hlt::Location> candidates;

            auto place_corner = [&](hlt::Location center, double offset) -> bool {
                for (auto i = 0; i < 3; i++) {
                    const auto angle = i * 2 * M_PI / 3 + offset;
                    const auto location = hlt::Location{
                        center.pos_x + distance_from_center * std::cos(angle),
                        center.pos_y + distance_from_center * std::sin(angle),
                    };
                    if (!is_ok_location(location, radius)) {
                        return false;
                    }
                    candidates.push_back(location);
                }
                return true;
            };

            if ((place_corner({5 * radius, 5 * radius}, -M_PI / 4) &&
                place_corner({map.map_width - 5 * radius, 5 * radius}, M_PI / 3) &&
                place_corner({5 * radius, map.map_height - 5 * radius}, M_PI / 4 - M_PI / 3) &&
                place_corner({map.map_width - 5 * radius, map.map_height - 5 * radius}, M_PI / 3 - M_PI / 4)
            )) {
                for (const auto location : candidates) {
                    map.planets.emplace_back(location.pos_x, location.pos_y, radius);
                }
            }
        }

        const size_t ship_count = hlt::GameConstants::get().SHIPS_PER_PLAYER;
        for (hlt::PlayerId player_id = 0; player_id < num_players;
             player_id++) {
            // Spread out ships to make it less likely they'll collide
            // in the start
            const auto& zone = spawn_zones[player_id];
            const int base_offset = 3;
            int offset = 0;
            for (size_t i = 0; i < ship_count; i++) {
                map.spawn_ship(hlt::Location{
                    zone.location.pos_x,
                    zone.location.pos_y + offset,
                }, player_id);
                offset = (offset >= 0) ? -offset - base_offset : -offset;
            }
        }

        return {};
    }

    auto SolarSystem::name() -> std::string {
        return "SolarSystem";
    }
}

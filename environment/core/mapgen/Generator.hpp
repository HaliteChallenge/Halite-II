//
// Created by David Li on 6/15/17.
//

#ifndef HALITE_GENERATOR_H
#define HALITE_GENERATOR_H

#ifdef _WIN32
#define _USE_MATH_DEFINES
#include <cmath>
#endif

#include <random>
#include "../hlt.hpp"
#include "../json.hpp"

namespace mapgen {
    enum class PointOfInterestType {
        Orbit,
    };

    /**
     * A map feature or decoration, for use by visualizers.
     */
    struct PointOfInterest {
        PointOfInterestType type;

        union {
            struct { hlt::Location location; int x_axis; int y_axis; } orbit;
        } data;

        PointOfInterest(hlt::Location location, int x_axis, int y_axis) {
            type = PointOfInterestType::Orbit;
            data.orbit = { location, x_axis, y_axis };
        }
    };

    struct Zone {
        hlt::Location location;
        double radius;

        Zone(hlt::Location _location, double _radius)
            : location(_location), radius(_radius) {}
    };

    /**
     * Base class for Halite map generators.
     */
    class Generator {
    protected:
        std::mt19937 rng;

    public:
        Generator(unsigned int _seed);

        /**
         * Get the name of this map generator.
         * @return
         */
        virtual auto name() -> std::string = 0;

        /**
         * Given a map, fill it with planets and initial ships.
         *
         * @param map The map to use.
         * @param num_players The number of players on the map.
         * @param effective_players The number of players to generate the map for.
         */
        virtual auto generate(
            hlt::Map& map,
            unsigned int num_players,
            unsigned int effective_players) -> std::vector<PointOfInterest> = 0;
    };

    auto to_json(nlohmann::json& json, const PointOfInterest& poi) -> void;
}

#endif //HALITE_GENERATOR_H

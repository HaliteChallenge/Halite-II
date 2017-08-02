//
// Created by David Li on 6/15/17.
//

#include "Generator.hpp"

namespace mapgen {
    Generator::Generator(unsigned int _seed) {
        rng = std::mt19937(_seed);
    }

    auto to_json(nlohmann::json& json, const PointOfInterest& poi) -> void {
        switch (poi.type) {
            case PointOfInterestType::Orbit:
                json["type"] = "orbit";
                json["x"] = poi.data.orbit.location.pos_x;
                json["y"] = poi.data.orbit.location.pos_y;
                json["x_axis"] = poi.data.orbit.x_axis;
                json["y_axis"] = poi.data.orbit.y_axis;
        }
    }
}

#ifndef HALITE_ASTEROIDCLUSTER_HPP
#define HALITE_ASTEROIDCLUSTER_HPP

#include "Generator.hpp"

namespace mapgen {
    class AsteroidCluster : Generator {
    public:
        AsteroidCluster(unsigned int _seed);

        auto generate(
            hlt::Map& map,
            unsigned int num_players,
            unsigned int effective_players) -> std::vector<PointOfInterest>;

        auto name() -> std::string;
    };

}

#endif //HALITE_ASTEROIDCLUSTER_HPP

#ifndef HALITE_HLT_H
#define HALITE_HLT_H

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace hlt {
    typedef unsigned char PlayerId;
    typedef unsigned long EntityIndex;

    struct GameConstants {
        int PLANETS_PER_PLAYER = 6;
        unsigned int EXTRA_PLANETS = 4;

        int DRAG = 3;
        int MAX_SPEED = 30;
        int MAX_ACCELERATION = 10;

        unsigned short MAX_SHIP_HEALTH = 255;
        unsigned short BASE_SHIP_HEALTH = 127;
        unsigned short DOCKED_SHIP_REGENERATION = 32;

        unsigned int WEAPON_COOLDOWN = 1;
        int WEAPON_RADIUS = 5;
        int WEAPON_DAMAGE = 64;

        unsigned int MAX_DOCKING_DISTANCE = 4;
        unsigned int DOCK_TURNS = 5;
        int PRODUCTION_PER_SHIP = 100;

        static auto get_mut() -> GameConstants& {
            // Guaranteed initialized only once by C++11
            static GameConstants instance;
            return instance;
        }

        static auto get() -> const GameConstants& {
            return get_mut();
        }
    };

    struct Location {
        unsigned short pos_x, pos_y;
    };

    struct Velocity {
        short vel_x, vel_y;

        auto magnitude() const -> double;
        auto angle() const -> double;
    };

    static bool operator==(const Location& l1, const Location& l2) {
        return l1.pos_x == l2.pos_x && l1.pos_y == l2.pos_y;
    }

    struct Entity {
        Location location;
        unsigned short health;
        //! The radius of the entity, in terms of grid cells from the center.
        //! Ships have a radius of 0 (they occupy only the center). A planet
        //! with radius=1 will occupy 5 grid cells (the center and the four
        //! adjacent cells in the cardinal directions)
        unsigned short radius;

        bool is_alive() const {
            return health > 0;
        }
    };

    enum class DockingStatus {
        Undocked = 0,
        Docking = 1,
        Docked = 2,
        Undocking = 3,
    };

    struct Ship : Entity {
        Velocity velocity;

        unsigned int weapon_cooldown;

        DockingStatus docking_status;
        unsigned int docking_progress;
        EntityIndex docked_planet;
    };

    struct Planet : Entity {
        PlayerId owner;
        bool owned;

        unsigned short remaining_production;
        unsigned short current_production;
        unsigned short docking_spots;

        //! Contains IDs of all ships in the process of docking or undocking,
        //! as well as docked ships.
        std::vector<EntityIndex> docked_ships;
    };

    enum class EntityType {
        InvalidEntity,
        ShipEntity,
        PlanetEntity,
    };

    enum class MoveType {
        //! Noop is not user-specifiable - instead it's the default command,
        //! used to mean that no command was issued
        Noop = 0,
        Thrust,
        Dock,
        Undock,
    };

    struct Move {
        MoveType type;
        EntityIndex ship_id;

        union {
            struct { unsigned short thrust; unsigned short angle; } thrust;
            EntityIndex dock_to;
        } move;

        static auto dock(EntityIndex ship_id, EntityIndex dock_to) -> Move {
            Move move;
            move.type = MoveType::Dock;
            move.ship_id = ship_id;
            move.move.dock_to = dock_to;

            return move;
        }

        static auto thrust(EntityIndex ship_id, double angle,
                           unsigned short thrust) -> Move {
            Move move;
            move.type = MoveType::Thrust;
            move.ship_id = ship_id;
            move.move.thrust.thrust = thrust;
            move.move.thrust.angle =
                static_cast<unsigned short>(angle * 180 / M_PI);

            return move;
        }
    };

    class Map {
    public:
        std::unordered_map<PlayerId, std::unordered_map<EntityIndex, Ship>> ships;
        std::unordered_map<EntityIndex, Planet> planets;
        // Number of rows and columns, NOT maximum index.
        unsigned short map_width, map_height;

        Map(unsigned short width, unsigned short height) {
            map_width = width;
            map_height = height;
            ships = {};
            planets = {};
        }

        auto get_ship(PlayerId player, EntityIndex index) -> Ship& {
            return ships.at(player).at(index);
        }

        auto get_planet(EntityIndex index) -> Planet& {
            return planets.at(index);
        }

        auto location_with_delta(Location& location, int dx, int dy) -> std::pair<Location, bool> {
            const auto x = location.pos_x + dx;
            const auto y = location.pos_y + dy;

            if (x < 0 || x >= map_width || y < 0 || y >= map_height) {
                return std::make_pair(Location{}, false);
            }

            return std::make_pair(Location{
                static_cast<unsigned short>(x),
                static_cast<unsigned short>(y),
            }, true);
        }
    };

    namespace {
        auto get_string() -> std::string {
            std::string result;
            std::getline(std::cin, result);
            return result;
        }

        auto send_string(std::string text) -> void {
            // std::endl used to flush
            std::cout << text << std::endl;
        }
    }

    static Map* last_map = nullptr;
    static PlayerId my_tag = 0;
    static unsigned short map_width;
    static unsigned short map_height;

    static auto parse_ship(std::istream& iss)
    -> std::pair<EntityIndex, Ship> {
        EntityIndex ship_id;
        iss >> ship_id;

        Ship ship;
        iss >> ship.location.pos_x;
        iss >> ship.location.pos_y;
        iss >> ship.health;
        iss >> ship.velocity.vel_x;
        iss >> ship.velocity.vel_y;
        int docking_status;
        iss >> docking_status;
        ship.docking_status = static_cast<DockingStatus>(docking_status);
        iss >> ship.docked_planet;
        iss >> ship.docking_progress;
        iss >> ship.weapon_cooldown;

        return std::make_pair(ship_id, ship);
    }

    static auto parse_planet(std::istream& iss)
        -> std::pair<EntityIndex, Planet> {
        Planet planet = {};
        EntityIndex planet_id;

        iss >> planet_id;
        iss >> planet.location.pos_x;
        iss >> planet.location.pos_y;
        iss >> planet.health;
        iss >> planet.radius;
        iss >> planet.docking_spots;
        iss >> planet.current_production;
        iss >> planet.remaining_production;
        int owned;
        iss >> owned;
        if (owned == 1) {
            planet.owned = true;
            int owner;
            iss >> owner;
            planet.owner = static_cast<PlayerId>(owner);
        }
        else {
            planet.owned = false;
            int false_owner;
            iss >> false_owner;
        }

        int num_docked_ships;
        iss >> num_docked_ships;
        planet.docked_ships.reserve(num_docked_ships);
        for (auto j = 0; j < num_docked_ships; j++) {
            EntityIndex ship_id;
            iss >> ship_id;
            planet.docked_ships.push_back(ship_id);
        }

        return std::make_pair(planet_id, planet);
    }

    static auto parse_map(std::string& input) -> Map {
        auto map = Map(map_width, map_height);
        std::stringstream iss(input);

        int num_players;
        iss >> num_players;

        // Meaningless loop indices, used as bookkeeping
        for (auto i = 0; i < num_players; i++) {
            PlayerId player_id;
            int player_id_int;
            iss >> player_id_int;
            player_id = static_cast<PlayerId>(player_id_int);

            int num_ships;
            iss >> num_ships;

            map.ships[player_id] = {};
            for (auto j = 0; j < num_ships; j++) {
                const auto& ship_pair = parse_ship(iss);
                map.ships[player_id].insert(ship_pair);
            }
        }

        int num_planets;
        iss >> num_planets;
        for (auto i = 0; i < num_planets; i++) {
            const auto& planet_pair = parse_planet(iss);
            map.planets[planet_pair.first] = planet_pair.second;
        }

        return map;
    }

    static auto get_map() -> Map {
        auto input = get_string();
        return parse_map(input);
    }

    static auto send_moves(std::vector<Move>& moves) -> void {
        std::ostringstream oss;
        for (const auto& move : moves) {
            switch (move.type) {
                case MoveType::Noop:
                    continue;
                case MoveType::Undock:
                    oss << "u " << move.ship_id << " ";
                    break;
                case MoveType::Dock:
                    oss << "d " << move.ship_id << " "
                        << move.move.dock_to << " ";
                    break;
                case MoveType::Thrust:
                    oss << "t " << move.ship_id << " "
                        << move.move.thrust.thrust << " "
                        << move.move.thrust.angle << " ";
                    break;
            }
        }
        send_string(oss.str());
    }

    static auto initialize(std::string bot_name) -> std::pair<PlayerId, Map> {
        std::cout.sync_with_stdio(false);
        my_tag = static_cast<PlayerId>(std::stoi(get_string()));

        std::stringstream iss(get_string());
        iss >> map_width >> map_height;

        auto initial_map = get_map();

        send_string(bot_name);

        return std::make_pair(my_tag, initial_map);
    }

    static auto get_distance(Location l1, Location l2) -> double {
        const auto dx = l1.pos_x - l2.pos_x;
        const auto dy = l1.pos_y - l2.pos_y;
        return std::sqrt(dx * dx + dy * dy);
    }

    static auto get_angle(Location l1, Location l2) -> double {
        const auto dx = l2.pos_x - l1.pos_x;
        const auto dy = l2.pos_y - l1.pos_y;
        return std::atan2(dy, dx);
    }

    static auto orient_towards(const Ship& ship, const Entity& target) -> double {
        auto dx = target.location.pos_x - ship.location.pos_x;
        auto dy = target.location.pos_y - ship.location.pos_y;

        auto angle_rad = std::atan2(dy, dx);
        if (angle_rad < 0) {
            angle_rad += 2 * M_PI;
        }

        return angle_rad;
    }

    static auto can_dock(const Ship& ship, const Planet& planet) -> bool {
        return get_distance(ship.location, planet.location) <=
            GameConstants::get().MAX_DOCKING_DISTANCE;
    }
}

#endif //HALITE_HLT_H

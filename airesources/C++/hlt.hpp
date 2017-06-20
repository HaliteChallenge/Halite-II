#ifndef HALITE_HLT_H
#define HALITE_HLT_H

#include <algorithm>
#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace hlt {
    struct Log {
    private:
        std::ofstream file;

        auto initialize(std::string filename) -> void {
            if (!ENABLED) return;
            file.open(filename, std::ios::trunc | std::ios::out);
        }

    public:
        constexpr static auto ENABLED = true;

        static auto open(std::string filename) -> void {
            if (!ENABLED) return;
            get().initialize(filename);
        }

        static auto get() -> Log& {
            static Log instance{};
            return instance;
        }

        static auto log(std::string message) -> void {
            if (!ENABLED) return;
            get().file << message << '\n';
        }
    };

    typedef unsigned char PlayerId;
    typedef unsigned long EntityIndex;

    /** Global state */
    static PlayerId my_tag = 0;
    static unsigned short map_width;
    static unsigned short map_height;

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

        friend auto operator<< (std::ostream& ostream, const Location& location) -> std::ostream& {
            ostream << '(' << location.pos_x << ", " << location.pos_y << ')';
            return ostream;
        }
    };

    struct Velocity {
        short vel_x, vel_y;

        auto magnitude() const -> double {
            return sqrt(vel_x*vel_x + vel_y*vel_y);
        }

        auto angle() const -> double {
            return atan2(vel_y, vel_x);
        }
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
            auto angle_deg = static_cast<int>(angle * 180 / M_PI) % 360;
            if (angle_deg < 0) angle_deg += 360;
            move.move.thrust.angle =
                static_cast<unsigned short>(angle_deg);

            return move;
        }

        static auto thrust(EntityIndex ship_id,
                           std::pair<double, unsigned short> direction) -> Move {
            return thrust(ship_id, direction.first, direction.second);
        }
    };

    //! A way to uniquely identify an Entity, regardless of its type.
    struct EntityId {
    private:
        //! Planets are unowned, and have player ID == -1.
        int _player_id;
        int _entity_index;

        EntityId() {
            type = EntityType::InvalidEntity;
            _player_id = -1;
            _entity_index = -1;
        }

    public:
        EntityType type;

        auto is_valid() const -> bool {
            return type != EntityType::InvalidEntity &&
                _player_id >= -1 && _entity_index >= 0;
        }

        auto player_id() const -> PlayerId {
            return static_cast<PlayerId>(_player_id);
        }
        auto entity_index() const -> EntityIndex {
            return static_cast<EntityIndex>(_entity_index);
        }

        //! Construct an entity ID representing an invalid entity.
        static auto invalid() -> EntityId {
            return EntityId();
        }
        //! Construct an entity ID for the given planet.
        static auto for_planet(EntityIndex index) -> EntityId {
            auto result = EntityId();
            result.type = EntityType::PlanetEntity;
            result._entity_index = static_cast<int>(index);
            return result;
        }
        //! Construct an entity ID for the given ship.
        static auto for_ship(PlayerId player_id, EntityIndex index) -> EntityId {
            auto result = EntityId();
            result.type = EntityType::ShipEntity;
            result._player_id = player_id;
            result._entity_index = static_cast<int>(index);
            return result;
        }

        friend auto operator<< (std::ostream& ostream, const EntityId& id) -> std::ostream& {
            switch (id.type) {
                case EntityType::InvalidEntity:
                    ostream << "[Invalid ID]";
                    break;
                case EntityType::PlanetEntity:
                    ostream << "[Planet " << id.entity_index() << "]";
                    break;
                case EntityType::ShipEntity:
                    ostream << "[Ship " << static_cast<int>(id.player_id());
                    ostream << ' ' << id.entity_index() << "]";
                    break;
            }

            return ostream;
        }
        friend auto operator== (const EntityId& id1, const EntityId& id2) -> bool {
            return id1._player_id == id2._player_id && id1._entity_index == id2._entity_index;
        }
        friend auto operator!= (const EntityId& id1, const EntityId& id2) -> bool{
            return !(id1 == id2);
        }
    };

    class Map {
    public:
        std::unordered_map<PlayerId, std::unordered_map<EntityIndex, Ship>> ships;
        std::unordered_map<EntityIndex, Planet> planets;
        // Number of rows and columns, NOT maximum index.
        unsigned short map_width, map_height;

        std::vector<std::vector<EntityId>> occupancy_map;

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

        auto location_with_delta(const Location& location, int dx, int dy) -> std::pair<Location, bool> {
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

        auto generate_occupancy_map() -> void {
            occupancy_map = std::vector<std::vector<EntityId>>(
                map_width,
                std::vector<EntityId>(map_height, EntityId::invalid()));

            for (const auto& planet_pair : planets) {
                const auto planet_index = planet_pair.first;
                const auto& planet = planet_pair.second;
                for (int dx = -planet.radius; dx <= planet.radius; dx++) {
                    for (int dy = -planet.radius; dy <= planet.radius; dy++) {
                        const auto location_pair = location_with_delta(planet.location, dx, dy);
                        const auto valid_location = location_pair.second;
                        const auto location = location_pair.first;
                        if (valid_location &&
                            dx*dx + dy*dy <= planet.radius*planet.radius) {
                            occupancy_map[location.pos_x][location.pos_y] =
                                EntityId::for_planet(planet_index);
                        }
                    }
                }
            }

            for (const auto& player_ship_pair : ships) {
                const auto& player_ships = player_ship_pair.second;
                const auto player_id = player_ship_pair.first;
                for (const auto& ship_pair : player_ships) {
                    const auto& location = ship_pair.second.location;
                    const auto ship_id = ship_pair.first;
                    occupancy_map[location.pos_x][location.pos_y] =
                        EntityId::for_ship(player_id, ship_id);
                }
            }
        }

        auto log_occupancy_map() -> void {
            std::stringstream log_msg;
            for (int y = 0; y < map_height; y++) {
                for (int x = 0; x < map_width; x++) {
                    if (occupancy_map[x][y].is_valid()) {
                        log_msg << 'X';
                    }
                    else {
                        log_msg << '.';
                    }
                }
                log_msg << '\n';
            }

            Log::log(log_msg.str());
        }

        constexpr static auto FORECAST_STEPS = 64;
        constexpr static auto FORECAST_DELTA = 1.0 / FORECAST_STEPS;

        auto occupiable(int x, int y) const -> bool {
            if (x < 0 || x >= map_width || y < 0 || y >= map_height) {
                return false;
            }

            auto occupancy = occupancy_map[x][y];
            if (occupancy.is_valid() &&
                occupancy.type == EntityType::PlanetEntity) {
                return false;
            }

            return true;
        }

        auto pathable(const Location& start, const Location& target) const -> bool {
            if (!occupiable(target.pos_x, target.pos_y)) {
                return false;
            }

            auto dx = (target.pos_x - start.pos_x) * FORECAST_DELTA;
            auto dy = (target.pos_y - start.pos_y) * FORECAST_DELTA;

            for (auto step = 0; step < FORECAST_STEPS; step++) {
                auto x = start.pos_x + step * dx;
                auto y = start.pos_y + step * dy;

                if (!occupiable(static_cast<int>(x), static_cast<int>(y))) {
                    return false;
                }
            }

            return true;
        }

        auto forecast_collision(const Location& start, double angle,
                                unsigned short thrust) -> bool {
            double current_x = start.pos_x + 0.5;
            double current_y = start.pos_y + 0.5;
            auto dx = std::round(thrust * std::cos(angle)) * FORECAST_DELTA;
            auto dy = std::round(thrust * std::sin(angle)) * FORECAST_DELTA;

            for (int time = 1; time <= FORECAST_STEPS; time++) {
                current_x += dx;
                current_y += dy;

                auto xp = static_cast<int>(current_x);
                auto yp = static_cast<int>(current_y);

                if (xp < 0 || xp >= map_width || yp < 0 || yp >= map_height) {
                    return true;
                }

                if (xp == start.pos_x && yp == start.pos_y) {
                    continue;
                }

                auto occupancy = occupancy_map[xp][yp];
                if (occupancy.is_valid()) {
                    return true;
                }
            }

            return false;
        }

        auto adjust_for_collision(
            const Location& start, double angle, unsigned short thrust,
            int tries=25) -> std::pair<double, unsigned short> {
            while (tries > 0) {
                if (forecast_collision(start, angle, thrust)) {
                    std::stringstream log_msg;
                    log_msg << "Forecasted collision for " << start << " at angle " << angle << " at thrust " << thrust;
                    Log::log(log_msg.str());
                    angle += M_PI / 12;
                }
                else {
                    std::stringstream log_msg;
                    log_msg << "No forecasted collision for " << start << " at angle " << angle << " at thrust " << thrust;
                    Log::log(log_msg.str());
                    break;
                }

                tries--;
            }

            return std::make_pair(angle, thrust);
        }
    };

    static auto get_distance(Location l1, Location l2) -> double {
        const auto dx = l1.pos_x - l2.pos_x;
        const auto dy = l1.pos_y - l2.pos_y;
        return std::sqrt(dx * dx + dy * dy);
    }

    static auto orient_towards(const Location& start, const Location& target) -> double {
        auto dx = target.pos_x - start.pos_x;
        auto dy = target.pos_y - start.pos_y;

        auto angle_rad = std::atan2(dy, dx);
        if (angle_rad < 0) {
            angle_rad += 2 * M_PI;
        }

        return angle_rad;
    }

    static auto orient_towards(const Ship& ship, const Entity& target) -> double {
        return orient_towards(ship.location, target.location);
    }

    static auto can_dock(const Ship& ship, const Planet& planet) -> bool {
        return get_distance(ship.location, planet.location) <=
            GameConstants::get().MAX_DOCKING_DISTANCE + planet.radius;
    }

    // TODO: This API could be made more convenient/as a member function
    auto closest_point(Map& game_map, const Location& start,
                       const Location& target, unsigned short radius)
        -> std::pair<Location, bool> {
        auto angle = orient_towards(start, target) + M_PI;
        auto dx = static_cast<int>(radius * std::cos(angle));
        auto dy = static_cast<int>(radius * std::sin(angle));

        return game_map.location_with_delta(target, dx, dy);
    }

    enum class BehaviorType {
        Brake,
        Warp,
    };

    struct Behavior {
        BehaviorType type;
        EntityIndex ship_id;

        union {
            struct { Location target; bool braked; bool braking; } warp;
        } data;

        auto is_finished(Map& game_map) const -> bool {
            if (game_map.ships[my_tag].count(ship_id) == 0) {
                return true;
            }

            const auto& ship = game_map.get_ship(my_tag, ship_id);
            switch (type) {
                case BehaviorType::Brake:
                    return ship.velocity.vel_x == 0 &&
                        ship.velocity.vel_y == 0;
                case BehaviorType::Warp:
                    return ship.location == data.warp.target;
            }
        }

        auto brake(double speed, double angle, int max_accel) -> Move {
            auto thrust = std::min(
                static_cast<unsigned short>(max_accel),
                static_cast<unsigned short>(speed));
            return Move::thrust(ship_id, angle + M_PI, thrust);
        }

        auto next(Map& game_map) -> Move {
            if (game_map.ships[my_tag].count(ship_id) == 0) {
                assert(false);
            }

            const auto& ship = game_map.get_ship(my_tag, ship_id);
            const auto max_accel = GameConstants::get().MAX_ACCELERATION;
            auto speed = ship.velocity.magnitude();
            auto angle = ship.velocity.angle();
            switch (type) {
                case BehaviorType::Brake: {
                    return brake(speed, angle, max_accel);
                }
                case BehaviorType::Warp: {
                    auto distance = get_distance(ship.location, data.warp.target);
                    auto turns_left = 10000;
                    if (speed > 0) {
                        turns_left = static_cast<int>(distance / speed);
                    }
                    auto turns_to_decelerate =
                        speed /
                            (GameConstants::get().MAX_ACCELERATION
                                + GameConstants::get().DRAG);

                    if (data.warp.braked || (data.warp.braking && speed == 0)) {
                        data.warp.braked = true;
                        // Move at low speed to target
                        const auto angle = orient_towards(ship.location, data.warp.target);
                        return hlt::Move::thrust(
                            ship_id,
                            game_map.adjust_for_collision(ship.location, angle, 2));
                    }
                    else if (turns_left <= turns_to_decelerate || data.warp.braking) {
                        // Start braking
                        data.warp.braking = true;
                        return brake(speed, angle, max_accel);
                    }
                    else {
                        // Accelerate
                        auto angle = orient_towards(ship.location, data.warp.target);
                        auto thrust = static_cast<unsigned short>(
                            std::max(1, std::min(
                                max_accel,
                                static_cast<int>(distance / 30 * max_accel))));
                        return Move::thrust(ship_id, angle, thrust);
                    }
                }
            }
        }

        auto cancel() -> void {
            type = BehaviorType::Brake;
        }
    };

    struct BehaviorManager {
        std::unordered_map<EntityIndex, Behavior> behaviors;

        BehaviorManager() {
            behaviors = {};
        }

        auto update(Map& game_map, std::vector<Move>& moves) -> void {
            for (auto& behavior_pair : behaviors) {
                auto& behavior = behavior_pair.second;
                if (!behavior.is_finished(game_map)) {
                    moves.push_back(behavior.next(game_map));
                }
            }

            for (auto it = std::begin(behaviors); it != std::end(behaviors);) {
                if (it->second.is_finished(game_map)) {
                    it = behaviors.erase(it);
                }
                else {
                    ++it;
                }
            }
        }

        auto is_behaving(EntityIndex ship_id) -> bool {
            return behaviors.count(ship_id) > 0;
        }

        auto warp_to(EntityIndex ship_id, Location& target) -> void {
            Behavior warp;
            warp.ship_id = ship_id;
            warp.type = BehaviorType::Warp;
            warp.data.warp.target = target;
            warp.data.warp.braked = false;
            warp.data.warp.braking = false;

            behaviors[ship_id] = warp;
        }
    };

    static auto get_string() -> std::string {
        std::string result;
        std::getline(std::cin, result);
        return result;
    }

    static auto send_string(std::string text) -> void {
        // std::endl used to flush
        std::cout << text << std::endl;
    }

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
        Log::log(oss.str());
        send_string(oss.str());
    }

    static auto initialize(std::string bot_name) -> std::pair<PlayerId, Map> {
        std::cout.sync_with_stdio(false);
        my_tag = static_cast<PlayerId>(std::stoi(get_string()));

        Log::open(std::to_string(static_cast<int>(my_tag)) + bot_name + ".log");

        std::stringstream iss(get_string());
        iss >> map_width >> map_height;

        auto initial_map = get_map();

        send_string(bot_name);

        return std::make_pair(my_tag, initial_map);
    }
}

#endif //HALITE_HLT_H

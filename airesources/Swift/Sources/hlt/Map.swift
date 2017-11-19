import Foundation

public enum EntityTypes {
    case ships
    case planets
    case planetsAndShips
}

public struct Map {
    let playerId: Int
    
    /// The width of the map
    public let width: Int
    
    /// The height of the map
    public let height: Int
    
    /// A list of all the players
    public let planets: [Planet]
    
    /// A list of all the planets
    public let players: [Player]

    
    init(playerId: Int, width: Int, height: Int, players: [Player], planets: [Planet]) {
        self.playerId = playerId
        self.width = width
        self.height = height
        self.planets = planets
        self.players = players
    }
    
    static func parse(_ tokens: TokenStack, playerId: Int, width: Int, height: Int) -> Map {
        let numPlayers = Int(tokens.pop())!
        
        // Deserialize all players
        var players = [Player]()
        
        for _ in 0..<numPlayers {
            players.append(Player.deserialize(tokens))
        }
        
        // Deserialize all planets
        let numPlanets = Int(tokens.pop())!
        var planets = [Planet]()
        
        for _ in 0..<numPlanets {
            planets.append(Planet.deserialize(tokens))
        }
        
        return Map(playerId: playerId, width: width, height: height, players: players, planets: planets)
    }
    
    func playersById() -> [Int: Player] {
        let ids = players.map { $0.playerId }
        return [Int: Player](uniqueKeysWithValues: zip(ids, players))
    }
    
    /// Returns the player
    public func getMe() -> Player {
        return getPlayer(playerId)!
    }
    
    /// Gets a player with the given id. Returns nil if it doesn't exist
    public func getPlayer(_ id: Int) -> Player? {
        return playersById()[id]
    }
    
    func planetsById() -> [Int: Planet] {
        let ids = planets.map { $0.id }
        return [Int: Planet](uniqueKeysWithValues: zip(ids, planets))
    }
    
    /// Gets a planet with the given id. Returns nil if it doesn't exist
    public func getPlanet(_ id: Int) -> Planet? {
        return planetsById()[id]
    }
    
    func allShips() -> [Ship] {
        return players.flatMap { $0.ships }
    }
    
    func allEntities() -> [Entity] {
        return (allShips() as [Entity]) + (planets as [Entity])
    }
    
    /// Returns an array of tuples of (distance, entity) with all entities sorted by their distance from the target
    public func entitiesByDistance(toEntity target: Entity) -> [(distance: Double, entity: Entity)] {
        let entities = allEntities().filter { $0.id != target.id }
        let distances: [Double] = entities.map {
            let dx = $0.x - target.x
            let dy = $0.y - target.y
            return sqrt(pow(dx, 2) + pow(dy, 2))
        }
        
        return zip(distances, entities).map {
            (distance: $0.0, entity: $0.1)
        }.sorted {
            $0.distance < $1.distance
        }
    }
    
    /// Returns an array of entities between the start entity and the given target. Optionally takes a parameter
    /// that specifies which types of entity to take into account
    public func obstaclesBetween(start: Entity, target: Entity, types: EntityTypes = .planetsAndShips) -> [Entity] {
        var entitiesFound = [Entity]()
        
        if types == .planetsAndShips || types == .planets {
            addEntitiesBetween(entitiesFound: &entitiesFound, start: start, target: target, entitiesToCheck: planets)
        }
        
        if types == .planetsAndShips || types == .ships {
            addEntitiesBetween(entitiesFound: &entitiesFound, start: start, target: target, entitiesToCheck: allShips())
        }
        
        return entitiesFound
    }
    
    private func addEntitiesBetween(entitiesFound: inout [Entity],
                                    start: Entity,
                                    target: Entity,
                                    entitiesToCheck: [Entity]) {
        for entity in entitiesToCheck {
            if entity == start || entity == target {
                continue
            }
            
            if intersectSegmentCircle(start: start, end: target, circle: entity, fudge: Constants.ForecastFudgeFactor) {
                entitiesFound.append(entity)
            }
        }
    }
}

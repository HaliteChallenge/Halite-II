import Foundation

public struct Planet: Entity, HLTDeserializable {
    /// The id of this planet
    public let id: Int
    
    /// The x coordinate of the planet
    public let x: Double
    
    /// The y coordinate of the planet
    public let y: Double
    
    /// The health of the planet
    public let health: Int
    
    /// The radius of the planet
    public let radius: Double
    
    /// The number of docking spots that the planet has
    public let dockingSpots: Int
    
    /// The current production of the planet
    public let currentProduction: Int
    
    /// The remaining production of the planet
    public let remainingProduction: Int
    
    /// Whether the planet is owned or not
    public let owned: Bool
    
    /// If the planet is owned, the id of the player that owns it, otherwise, nil
    public let ownerId: Int?
    
    /// The number of ships docked at the planet
    public let dockedShipCount: Int
    
    /// The ids of the ships docked at the planet
    public let dockedShipIds: [Int]
    
    static func deserialize(_ tokens: TokenStack) -> Planet {
        let id = Int(tokens.pop())!
        let x = Double(tokens.pop())!
        let y = Double(tokens.pop())!
        let health = Int(tokens.pop())!
        let radius = Double(tokens.pop())!
        let dockingSpots = Int(tokens.pop())!
        let currentProduction = Int(tokens.pop())!
        let remainingProduction = Int(tokens.pop())!
        let owned = (Int(tokens.pop())! != 0) ? true: false
        let ownerId = Int(tokens.pop())!
        let dockedShipCount = Int(tokens.pop())!
        var dockedShipIds = [Int]()
        
        for _ in 0..<dockedShipCount {
            dockedShipIds.append(Int(tokens.pop())!)
        }
        
        return Planet(id: id,
                      x: x,
                      y: y,
                      health: health,
                      radius: radius,
                      dockingSpots: dockingSpots,
                      currentProduction: currentProduction,
                      remainingProduction: remainingProduction,
                      owned: owned,
                      ownerId: owned ? ownerId : nil,
                      dockedShipCount: dockedShipCount,
                      dockedShipIds: dockedShipIds)
    }
}

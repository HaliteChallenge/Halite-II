import Foundation

public struct Player: HLTDeserializable {
    
    /// The ID of the player
    public let playerId: Int
    
    /// The ships owned by the player
    public private(set) var ships:   [Ship] = []
    
    static func deserialize(_ tokens: TokenStack) -> Player {
        let playerId = Int(tokens.pop())!
        
        let numShips = Int(tokens.pop())!
        var ships = [Ship]()
        
        for _ in 0..<numShips {
            ships.append(Ship.deserialize(tokens))
        }
        
        return Player(playerId: playerId, ships: ships)
    }
    
}

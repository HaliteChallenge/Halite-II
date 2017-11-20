import Foundation

public class Game {
    let botName: String
    let sendName: Bool
    let playerId: Int
    
    let mapWidth: Int
    let mapHeight: Int
    
    public var map: Map
    var currentTurn = 0
    
    public var log: FileLogger
    
    /// Initialises the game with the given bot name
    public init(botName: String) {
        self.botName = botName
        self.sendName = false
        self.playerId = Int(Game.getString())!
        
        // Set up logging
        let logFilePath = "\(playerId)_\(botName).log"
        log = FileLogger(path: logFilePath, logLevel: .info)

        log.info("Initialized bot \(botName) with id \(playerId)")
        
        // Get width and height of map
        let mapSizeComponents = Game.getString()
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .split(separator: " ")
            .map { Int($0)! }
        
        self.mapWidth = mapSizeComponents[0]
        self.mapHeight = mapSizeComponents[1]
        
        log.info("Map size: \(mapWidth) \(mapHeight)")
        
        self.map = Map(playerId: playerId, width: 0, height: 0, players: [], planets: [])
        
        // Initial map update
        _ = updateMap()
    }
    
    /// Starts a new turn, returns the updated map
    public func updateMap() -> Map {
        // Before the first turn, send the bot name
        if currentTurn == 1 {
            sendCommand(self.botName)
        }
        
        log.info("---TURN \(currentTurn)---")
        
        let mapList = Game.getString()
        let tokens = TokenStack(mapList.split(separator: " ").map { String($0) })
        self.map = Map.parse(tokens, playerId: playerId, width: mapWidth, height: mapHeight)
        
        currentTurn += 1
        
        return self.map
    }
    
    /// Send the given list of moves for a turn
    public func sendMoves(_ moves: [Move]) {
        sendCommand(Move.serializeMoves(moves))
    }
    
    static func getString() -> String {
        guard let input = readLine(strippingNewline: true) else {
            fatalError("EOF when expecting input")
        }
        
        return input
    }
    
    func sendCommand(_ cmd: String) {
        log.info("Sending command: \(cmd)")
        print(cmd)
        
        // Need a flush here.
        fflush(stdout)
    }
}

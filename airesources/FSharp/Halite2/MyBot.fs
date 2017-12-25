open System
open hlt

[<EntryPoint>]
let main argv =
    let botName = 
        match argv.Length with 
            | x when x > 0 -> argv.[0] 
            | _ -> "Ephesus"

    let conn = newConnection botName

    let mutable gameMap = updateMap conn
    let mutable gameTurn = 1

    while true do
        gameMap <- updateMap conn

        let myPlayer = 
            gameMap.Players 
                |> Array.find (fun p -> p.Id = gameMap.MyId)
        
        let commandQueue = 
            myPlayer.Ships
                |> Array.filter (fun s -> s.DockingStatus = Undocked)
                |> Array.map (fun s -> strategyBasicBot s gameMap)

        submitCommands commandQueue

        gameTurn <- gameTurn + 1      

    0 // return an integer exit code

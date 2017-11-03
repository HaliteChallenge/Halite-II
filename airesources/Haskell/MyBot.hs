import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap
import Hlt.Navigation
import Hlt.Networking

-- | Define the name of our bot.
botName :: String
botName = "AdamBot"

-- | Log to a file
info :: GameMap -> String -> IO ()
info g s = appendFile (show (myId g) ++ "-" ++ botName ++ ".log") (s ++ "\n")

{-|
  Calculates a move for one Ship.

  Simply navigates to the closest point on the first Planet in the given list.
-}
calcMove :: GameMap -> [Planet] -> Ship -> String
calcMove g ps s = do
    let p = head ps
    if canDock s p then
        dock s p
    else
        navigateToTarget g (maxSpeed/2) False 180 s (closestLocationTo s p)

-- | The primary function for controlling the game turns.
run :: GameMap -> IO ()
run i = do
    -- Update map
    g <- updateGameMap i
    info g "---NEW TURN---"

    let ss = filter isUndocked (listMyShips g)         -- all undocked Ships of mine
        ps = filter (not . isOwned) (listAllPlanets g) -- all unowned Planets on the map

    -- Send commands to move each Ship to the first empty Planet
    sendCommands (if length ps > 0 then map (\a -> calcMove g ps a) ss else [""])

    -- Go to next turn
    run g

-- | Main function where we initialize our bot and call the run function.
main :: IO ()
main = do
    i <- initialGameMap

    -- You can analyse the initial map (i) here, 60 seconds time limit

    sendString botName
    info i ("Initialized bot " ++ botName)
    run i

import Hlt.Entity
import Hlt.GameMap
import Hlt.Navigation
import Hlt.Networking

-- | Define the name of our bot.
botName :: String
botName = "AdamBot"

-- | Log to a file
info :: String -> IO ()
info s = appendFile (botName ++ ".log") (s ++ "\n")

{-|
  Calculates a move for one Ship.

  Simply navigates to the closest point on the first planet in the given list.
-}
calcMove :: GameMap -> [Entity] -> Entity -> String
calcMove g ps s = do
    let p = ps !! 0
    if canDock s p then dockCommand s p
    else navigateToTarget g 180 s (getClosestPointTo s p)

-- | The primary function for controlling the game turns.
run :: GameMap -> IO ()
run g = do
    let ss = filter isUndocked (getMyShips g) -- all undocked Ships of mine
        ps = filter (not . isOwned) (getAllPlanets g) -- all unowned Planets on the map

    -- Send each Ship to the first empty planet
    sendCommands (if length ps > 0 then map (\a -> calcMove g ps a) ss else [""])

    -- Update map for next iteration
    a <- updateGameMap g
    run a

-- | Main function where we initialize our bot and call the run function.
main :: IO ()
main = do
    i <- initialize botName
    -- You can pre analyse the initial map (i) here
    g <- updateGameMap i
    run g

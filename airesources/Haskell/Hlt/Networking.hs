-- | Communicate with the Halite game environment.
module Hlt.Networking where

import qualified Data.Map as Map
import Text.Read
import System.IO
import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap

instance Read PlayerId where
    readPrec = do
      a <- step readPrec :: ReadPrec Int
      return $ PlayerId a

instance Read Player where
    readPrec = do
        i <- step readPrec :: ReadPrec PlayerId -- id
        n <- step readPrec :: ReadPrec Int      -- number of ships
        let next c = if c == 0 then
                         return []
                     else do
                         s <- step readPrec :: ReadPrec Ship
                         ss <- next (c-1)
                         return $ (shipId s, s):ss
        s <- next n                             -- list of ships
        return $ Player i (Map.fromList s)

instance Read ShipId where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ ShipId a

instance Read DockingStatus where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ case a of
            0 -> Undocked
            1 -> Docking
            2 -> Docked
            3 -> Undocking

instance Read Ship where
    readPrec = do
        i <- step readPrec :: ReadPrec ShipId        -- id
        j <- step readPrec :: ReadPrec Float         -- x
        k <- step readPrec :: ReadPrec Float         -- y
        h <- step readPrec :: ReadPrec Int           -- health
        _ <- step readPrec :: ReadPrec Float         -- x velocity (deprecated)
        _ <- step readPrec :: ReadPrec Float         -- y velocity (deprecated)
        s <- step readPrec :: ReadPrec DockingStatus -- docking status
        p <- step readPrec :: ReadPrec PlanetId      -- docked planet
        d <- step readPrec :: ReadPrec Int           -- docking progress
        c <- step readPrec :: ReadPrec Int           -- weapon cooldown
        return $ Ship i j k h s (if s /= Undocked then Just p else Nothing) d c

instance Read PlanetId where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ PlanetId a

instance Read Planet where
    readPrec = do
        i <- step readPrec :: ReadPrec PlanetId -- id
        j <- step readPrec :: ReadPrec Float    -- x
        k <- step readPrec :: ReadPrec Float    -- y
        h <- step readPrec :: ReadPrec Int      -- health
        r <- step readPrec :: ReadPrec Float    -- radius
        d <- step readPrec :: ReadPrec Int      -- docking spots
        p <- step readPrec :: ReadPrec Int      -- production
        _ <- step readPrec :: ReadPrec Int      -- remaining production (deprecated)
        w <- step readPrec :: ReadPrec Int      -- whether it is owned
        o <- step readPrec :: ReadPrec PlayerId -- the player id if it is owned
        s <- step readPrec :: ReadPrec Int      -- number of docked ships
        let next c = if c == 0 then return []
                     else do
                        s <- step readPrec :: ReadPrec ShipId
                        ss <- next (c-1)
                        return $ s:ss
        l <- next s                             -- list if ship ids
        return $ Planet i j k h r d p (if w == 1 then Just o else Nothing) l

-- | Read a list of Players and Ships from a string,
readPlayers :: Int -> String -> (([(PlayerId, Player)], String))
readPlayers 0 s = ([], s)
readPlayers c s = do
    let (a, r0) = head $ (reads :: ReadS Player) s
        (b, r1) = readPlayers (c-1) r0
    ((playerId a, a):b, r1)

-- | Read a list of Planets from a string,
readPlanets :: Int -> String -> (([(PlanetId, Planet)], String))
readPlanets 0 s = ([], s)
readPlanets c s = do
    let (a, r0) = head $ (reads :: ReadS Planet) s
        (b, r1) = readPlanets (c-1) r0
    ((planetId a, a):b, r1)

-- | Get the next int and return the rest of the string.
nextInt :: String -> (Int, String)
nextInt s = head $ (reads :: ReadS Int) s

-- | Read players and planets from stdin.
readGameMap :: IO (Map.Map PlayerId Player,  Map.Map PlanetId Planet)
readGameMap = do
    line <- getLine
    let (c0, r0) = nextInt line
        (p0, r1) = readPlayers c0 r0
        (c1, r2) = nextInt r1
        (p1, r3) = readPlanets c1 r2
    return (Map.fromList p0, Map.fromList p1)

-- | Send a string to the game environment.
sendString :: String -> IO ()
sendString s = do
    putStrLn s
    hFlush stdout

-- | Send a list of commands to the game environment.
sendCommands :: [String] -> IO ()
sendCommands = sendString . unwords

-- | Get the initial GameMap.
initialGameMap :: IO GameMap
initialGameMap = do
    line0 <- getLine -- our player id
    line1 <- getLine -- game dimensions
    let i = read line0 :: PlayerId
        (w, h) = (\[a, b] -> (a, b)) $ map read $ words line1
    (p0, p1) <- readGameMap
    return (GameMap i w h p0 p1)

-- | Get the new GameMap.
updateGameMap :: GameMap -> IO GameMap
updateGameMap g = do
    (p0, p1) <- readGameMap
    return (GameMap (myId g) (width g) (height g) p0 p1)

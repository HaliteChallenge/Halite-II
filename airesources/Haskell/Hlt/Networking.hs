-- | Communicate with the Halite game environment.
module Hlt.Networking where

import qualified Data.Map as Map
import System.IO
import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap

-- | Return first element and the rest of the list.
splitAtOne :: [String] -> (Int, [String])
splitAtOne (a:as) = (read a, as)

-- | Extract Planet from String List, prepend to List and return rest.
extractPlanet :: [String] -> ((Int, Entity), [String])
extractPlanet (i:j:k:h:r:d:p:_:c:o:s:rest) = (\(a, b) -> ((read i, Planet (read i) (read j) (read k) (read h) (read r) (read d) (read p) (if (read c :: Int) == 1 then (read o) else (-1)) (map read a)), b)) (splitAt (read s :: Int) rest)

-- | Extract all Planets from String List.
extractAllPlanets :: [(Int, Entity)] -> Int -> [String] -> ([(Int, Entity)], [String])
extractAllPlanets p 0 r= (p, r)
extractAllPlanets p n r = (\(a, b) -> (extractAllPlanets (a:p) (n - 1) b)) $ extractPlanet r

-- | Extract Ship from String List, prepend to List and return rest.
extractShip :: [String] -> Int -> ((Int, Entity), [String])
extractShip (i:j:k:h:_:_:s:p:d:c:r) o = ((read i :: Int, Ship (read i) (read j) (read k) (read h) shipRadius o (read s) (read p) (read d) (read c)), r)

-- | Extract all Ships from String List.
extractAllShips :: [(Int, Entity)] -> Int -> [String] -> Int -> ([(Int, Entity)], [String])
extractAllShips s 0 r _ = (s, r)
extractAllShips s n r o =  (\(a, b) -> (extractAllShips (a:s) (n - 1) b o)) $ extractShip r o

-- | Extract Player from String List, prepend to List and return rest.
extractPlayer :: [String] -> ((Int, Player), [String])
extractPlayer (i:r) = (\(a, b) -> ((read i :: Int, Player (read i) (Map.fromList a)), b)) $ (\(c, d) -> extractAllShips [] c d (read i)) $ splitAtOne r

-- | Extract all Players from String List.
extractAllPlayers :: [(Int, Player)] -> Int -> [String] -> ([(Int, Player)], [String])
extractAllPlayers p 0 r = (p, r)
extractAllPlayers p n r = (\(a, b) -> (extractAllPlayers (a:p) (n - 1) b)) $ extractPlayer r

-- | Read my ID from stdin.
readId :: IO Int
readId = do
    line <- getLine
    return $ read line

-- | Read the width and height from stdin.
readDimensions :: IO (Float, Float)
readDimensions = do
    line <- getLine
    return $ (\[a, b] -> (a, b)) $ map read $ words line

-- | Read game map from stdin.
readGameMap :: IO (Map.Map Int Player,  Map.Map Int Entity)
readGameMap = do
    raw <- getLine
    let (players, rest) = (\(a, b) -> extractAllPlayers [] a b) $ splitAtOne $ words raw
    let (planets, _) = (\(a, b) -> extractAllPlanets [] a b) $ splitAtOne rest
    return (Map.fromList players, Map.fromList planets)

-- | Send a String to the game environment.
sendString :: String -> IO ()
sendString s = do
    putStrLn s
    hFlush stdout

-- | Send a List of commands to the game environment.
sendCommands :: [String] -> IO ()
sendCommands = sendString . unwords

-- | Initialize bot.
initialize :: String -> IO GameMap
initialize n = do
    i <- readId
    (w, h) <- readDimensions
    sendString n
    (players, planets) <- readGameMap
    return (GameMap i w h players planets)

-- | Get the new game frame.
updateGameMap :: GameMap -> IO GameMap
updateGameMap g = do
    (players, planets) <- readGameMap
    return (GameMap (myId g) (width g) (height g) players planets)

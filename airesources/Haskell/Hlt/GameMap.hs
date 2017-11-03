-- | A data type representing the game map at a particular frame, and functions that operate on it.
module Hlt.GameMap where

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Hlt.Entity

-- | A GameMap contains the current frame of a Halite game.
data GameMap = GameMap { myId :: PlayerId
                       , width :: Int
                       , height :: Int
                       , allPlayers :: Map.Map PlayerId Player
                       , allPlanets :: Map.Map PlanetId Planet
                       }
              deriving (Show)

-- | Return a list of all Planets.
listAllPlanets :: GameMap -> [Planet]
listAllPlanets g = Map.elems $ allPlanets g

-- | Return a list of all Ships.
listAllShips :: GameMap -> [Ship]
listAllShips g = concat $ map (Map.elems . ships) (Map.elems $ allPlayers g)

-- | Return a list of my Ships.
listMyShips :: GameMap -> [Ship]
listMyShips g = Map.elems $ ships $ Maybe.fromJust $ Map.lookup (myId g) (allPlayers g)

-- | Checks if any of the given Entities are in between two Entities.
entitiesBetweenList :: Entity a => Entity b => Entity c => [a] -> b -> c -> Bool
entitiesBetweenList l e0 e1 =  any (\e -> isSegmentCircleCollision e0 e1 e) $ filter (\e -> notEqual e e0 && notEqual e e1) l

-- | Checks if there are any Entities between two Entities.
entitiesBetween :: Entity a => Entity b => GameMap -> Bool -> a -> b -> Bool
entitiesBetween g c a b = entitiesBetweenList (listAllPlanets g) a b || (c && entitiesBetweenList (listAllShips g) a b)

-- | A data type representing the game map at a particular frame, and functions that operate on it.
module Hlt.GameMap where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Hlt.Entity

-- | A GameMap contains the current frame of a Halite game.
data GameMap = GameMap { myId :: Int
                       , width :: Float
                       , height :: Float
                       , allPlayers :: Map.Map Int Player
                       , allPlanets :: Map.Map Int Entity
                       }
              deriving (Show)


-- | Return a List of all Planets.
getAllPlanets :: GameMap -> [Entity]
getAllPlanets g = Map.elems $ allPlanets g

-- | Return a List of all Ships.
getAllShips :: GameMap -> [Entity]
getAllShips g = concat $ map (Map.elems . ships) (Map.elems $ allPlayers g)

-- | Return a List of my Ships.
getMyShips :: GameMap -> [Entity]
getMyShips g = Map.elems $ ships $ Maybe.fromJust $ Map.lookup (myId g) (allPlayers g)

-- | Return a List of all Entities.
getAllEntities :: GameMap -> [Entity]
getAllEntities g = getAllPlanets g ++ getAllShips g

-- | Check if there are Entities between two Entities.
entitiesBetween :: GameMap -> Entity -> Entity -> Bool
entitiesBetween g a b = any (\e -> isSegmentCircleCollision a b e) $ filter (\e -> e /= a && e /= b) (getAllEntities g)

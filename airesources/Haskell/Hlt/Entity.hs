-- | Defines Planets and Ships and various functions that operate on them.
module Hlt.Entity where

import qualified Data.Map as Map
import Hlt.Constants

-- | Grouping of a player (bot) and its corresponding Ships.
data Player = Player { playerId :: Int, ships :: Map.Map Int Entity } deriving (Show)

{-|
  An Entity is an object on the GameMap.

  All Entities have an (x, y) position and a radius.

  Planets and Ships both have corresponding health and owner attributes and various extra attributes.
-}
data Entity = Location { x :: Float
                       , y :: Float
                       , radius :: Float
                       }
            | Planet   { entityId :: Int
                       , x :: Float
                       , y :: Float
                       , health :: Int
                       , radius :: Float
                       , dockingSpots :: Int
                       , production :: Int
                       , owner :: Int
                       , dockedShips :: [Int]
                       }
            | Ship     { entityId :: Int
                       , x :: Float
                       , y :: Float
                       , health :: Int
                       , radius :: Float
                       , owner :: Int
                       , status :: Int
                       , planet :: Int
                       , dockingProgress :: Int
                       , weaponCooldown :: Int
                       }
            deriving (Show)

-- | Entities are equal if their locations on the GameMap are equal.
instance Eq Entity where
   a == b = (x a == x b && y a == y b)

-- | Returns the x component of the difference between two Entities.
dX :: Entity -> Entity -> Float
dX a b = x b - x a

-- | Returns the y component of the difference between two Entities.
dY :: Entity -> Entity -> Float
dY a b = y b - y a

-- | Returns the angle between two Entities.
getAngle :: Entity -> Entity -> Float
getAngle a b = atan2 (dY a b) (dX a b)

-- | Returns the distance between two Entities.
getDistance :: Entity -> Entity -> Float
getDistance a b = sqrt ((dX a b)**2 + (dY a b)**2)

-- | Returns the distance between the edges of two Entities.
getDistanceEdges :: Entity -> Entity -> Float
getDistanceEdges a b = getDistance a b - radius a - radius b

-- | Return the closest point (Circle) to Entity
getClosestPointTo :: Entity -> Entity -> Entity
getClosestPointTo s p = do
   let a = getAngle p s
       r = radius p + 3
   Location ((x p) + r * cos(a)) ((y p) + r * sin(a)) 0

-- | Checks if an Entity is owned by a Player.
isOwned :: Entity -> Bool
isOwned e = owner e /= -1

-- | Checks if a Ship is undocked.
isUndocked :: Entity -> Bool
isUndocked s = status s == 0

-- | Checks if a Ship is docking.
isDocking :: Entity -> Bool
isDocking s = status s == 1

-- | Checks if a Ship is docked.
isDocked :: Entity -> Bool
isDocked s = status s == 2

-- | Checks if a Ship is undocking.
isUndocking :: Entity -> Bool
isUndocking s = status s == 3

-- | Checks if a Planet is fully docked.
isFull :: Entity -> Bool
isFull p = length (dockedShips p) == dockingSpots p

-- | Checks if a Ship is within docking range of Planet.
canDock :: Entity -> Entity -> Bool
canDock s p = getDistance s p <= radius p + dockRadius

-- | Checks whether the line segment between a Ship and an Entity collide with a circle on the GameMap.
isSegmentCircleCollision :: Entity -> Entity -> Entity -> Bool
isSegmentCircleCollision a b c = do
   let dx = dX a b
       dy = dY a b
       j = dx**2 + dy**2
   if j == 0 then (getDistance a c <= (radius c) + 0.75)
   else (do
      let k = -2 * ((x a)**2 - (x a)*(x b) - (x a)*(x c) + (x b)*(x c) + (y a)**2 - (y a)*(y b) - (y a)*(y c) + (y b)*(y c))
          t = min (-k / (2 * j)) 1.0
      ((t >= 0) && ((getDistance (Location ((x a) + dx * t) ((y a) + dy * t) 0) c) <= (radius c) + 0.75)))

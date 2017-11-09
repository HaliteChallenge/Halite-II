-- | Defines Planets and Ships and various functions that operate on them.
module Hlt.Entity where

import qualified Data.Map as Map
import Text.Read
import Hlt.Constants

-- | Grouping of a Player (bot) and its corresponding Ships.
data Player = Player { playerId :: PlayerId, ships :: Map.Map ShipId Ship } deriving (Show)

-- | An Entity is an object on the GameMap.
class Entity t where
    x :: t -> Float
    y :: t -> Float
    radius :: t -> Float

newtype PlayerId = PlayerId Int
    deriving (Eq, Ord)

instance Show PlayerId where
    show (PlayerId i) = show i

newtype ShipId = ShipId Int
    deriving (Eq, Ord)

instance Show ShipId where
    show (ShipId i) = show i

newtype PlanetId = PlanetId Int
    deriving (Eq, Ord)

instance Show PlanetId where
    show (PlanetId i) = show i

data DockingStatus = Undocked | Docking | Docked | Undocking
     deriving (Eq, Show)

data Location = Location { locationX :: Float
                         , locationY :: Float
                         } deriving (Eq, Show)

data Planet = Planet { planetId :: PlanetId
                     , planetX :: Float
                     , planetY :: Float
                     , planetHealth :: Int
                     , planetRadius :: Float
                     , dockingSpots :: Int
                     , production :: Int
                     , planetOwner :: Maybe PlayerId
                     , dockedShips :: [ShipId]
                     } deriving (Eq, Show)

data Ship = Ship { shipId :: ShipId
                 , shipX :: Float
                 , shipY :: Float
                 , shipHealth :: Int
                 , dockingStatus :: DockingStatus
                 , planet :: Maybe PlanetId
                 , dockingProgress :: Int
                 , weaponCooldown :: Int
                 } deriving (Eq, Show)

instance Entity Location where
    x = locationX
    y = locationY
    radius = const 0

instance Entity Planet where
    x = planetX
    y = planetY
    radius = planetRadius

instance Entity Ship where
    x = shipX
    y = shipY
    radius = pure shipRadius

-- Check if two Entities are equal
equal :: Entity a => Entity b => a -> b -> Bool
equal a b = (x a == x b && y a == y b)

-- Check if two Entities are not equal
notEqual :: Entity a => Entity b => a -> b -> Bool
notEqual a b = not $ equal a b

-- | Returns the x component of the difference between two Entities.
dX :: Entity a => Entity b => a -> b -> Float
dX a b = x b - x a

-- | Returns the y component of the difference between two Entities.
dY :: Entity a => Entity b => a -> b -> Float
dY a b = y b - y a

-- | Returns the angle in radians between two Entities.
angleRadians :: Entity a => Entity b => a -> b -> Float
angleRadians a b = atan2 (dY a b) (dX a b)

-- | Returns the distance between two Entities.
distance :: Entity a => Entity b => a -> b -> Float
distance a b = sqrt ((dX a b)**2 + (dY a b)**2)

-- | Returns the distance between the edges of two Entities.
distanceEdges :: Entity a => Entity b => a -> b -> Float
distanceEdges a b = distance a b - radius a - radius b

-- | Return the closest Location to an Entity
closestLocationTo :: Entity a => Entity b => a -> b -> Location
closestLocationTo s p = do
   let a = angleRadians p s
       r = radius p + 3
   Location ((x p) + r * cos(a)) ((y p) + r * sin(a))

-- | Checks if an Planet is owned by a Player.
isOwned :: Planet -> Bool
isOwned p = planetOwner p /= Nothing

-- | Checks if a Ship is undocked.
isUndocked :: Ship -> Bool
isUndocked s = dockingStatus s == Undocked

-- | Checks if a Ship is docking.
isDocking :: Ship -> Bool
isDocking s = dockingStatus s == Docking

-- | Checks if a Ship is docked.
isDocked :: Ship -> Bool
isDocked s = dockingStatus s == Docked

-- | Checks if a Ship is undocking.
isUndocking :: Ship -> Bool
isUndocking s = dockingStatus s == Undocking

-- | Checks if a Planet is fully docked.
isFull :: Planet -> Bool
isFull p = length (dockedShips p) == dockingSpots p

-- | Checks if a Ship is within docking range of Planet.
canDock :: Ship -> Planet -> Bool
canDock s p = distance s p <= radius p + dockRadius + shipRadius

-- | Constant fudge value so that ships don't collide into to each other.
collisionFudgeFactor :: Float
collisionFudgeFactor = 0.75

-- | Checks whether the line segment between a Ship and an Entity collide with a Circle on the GameMap.
isSegmentCircleCollision :: Entity a => Entity b => Entity c => a -> b -> c -> Bool
isSegmentCircleCollision a b c = do
    let dx = dX a b
        dy = dY a b
        j = dx**2 + dy**2
    if j == 0 then
        (distance a c <= (radius c) + collisionFudgeFactor)
    else do
        let k = -2 * ((x a)**2 - (x a)*(x b) - (x a)*(x c) + (x b)*(x c) + (y a)**2 - (y a)*(y b) - (y a)*(y c) + (y b)*(y c))
            t = min (-k / (2 * j)) 1.0
        ((t >= 0) && ((distance (Location ((x a) + dx * t) ((y a) + dy * t)) c) <= (radius c) + collisionFudgeFactor))

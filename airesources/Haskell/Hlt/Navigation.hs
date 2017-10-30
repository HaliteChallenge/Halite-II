-- | Functions for Ship navigation.
module Hlt.Navigation where

import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap

-- | Converts angle in radians to degrees and truncates to Int.
radiansToDegrees :: Float -> Int
radiansToDegrees a = mod (round (a * 180 / pi)) 360

-- | Generates a thrust command.
thrustCommand :: Entity -> Float -> Float -> String
thrustCommand s v a = "t " ++ (show $ entityId s) ++ " " ++ (show $ (floor v :: Int)) ++ " " ++ (show $ radiansToDegrees a)

{-|
  Generates a dock command.

  example:

  > dockCommand ship planet
-}
dockCommand :: Entity -> Entity -> String
dockCommand s p = "d " ++ (show $ entityId s) ++ " " ++ (show $ entityId p)

{-|
  Generates an undock command.

  example:

  > dockCommand ship
-}
undockCommand :: Entity -> String
undockCommand s = "u " ++ (show $ entityId s)

{-|
  Move an entity directly towards a target.

  example:

  > dockCommand ship target
-}
moveToTarget :: Entity -> Entity -> String
moveToTarget s e = thrustCommand s (min (getDistanceEdges s e) (maxSpeed / 2)) (getAngle s e)

{-|
  Navigate an Entity towards a target.

  example:

  > dockCommand ship target corrections

  where `corrections` is the number of corrections to attempt when finding a path with no obstacles.
-}
navigateToTarget :: GameMap -> Int -> Entity -> Entity -> String
navigateToTarget g i s e = do
    let d = getDistance s e
        a = getAngle s e
        step = pi/90
    if (i < 0) then "" else
        if (entitiesBetween g s e) then navigateToTarget g (i - 1) s (Location (x s + (cos (a + step)) * d) (y s + (sin (a + step)) * d) (radius e))
        else thrustCommand s (min d (maxSpeed / 2)) a

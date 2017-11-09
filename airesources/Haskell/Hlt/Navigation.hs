-- | Functions for Ship navigation.
module Hlt.Navigation where

import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap

-- | Converts angle in radians to degrees and truncates to an int.
radiansToDegrees :: Float -> Int
radiansToDegrees a = mod (round (a * 180 / pi)) 360

-- | Generates a thrust command.
thrust :: Ship -> Float -> Float -> String
thrust s v a = "t " ++ (show $ shipId s) ++ " " ++ (show $ (floor v :: Int)) ++ " " ++ (show $ radiansToDegrees a)

-- | Generates a dock command.
dock :: Ship -> Planet -> String
dock s p = "d " ++ (show $ shipId s) ++ " " ++ (show $ planetId p)

-- | Generates an undock command.
undock :: Ship -> String
undock s = "u " ++ (show $ shipId s)

-- | Move a Ship directly towards a target.
moveToTarget :: Entity a => Ship -> a -> String
moveToTarget s e = thrust s (min (distanceEdges s e) (maxSpeed / 2)) (angleRadians s e)

{-|
  Navigate an Entity towards a target.

  example:

  > dockCommand gamemap speed include_ships corrections ship target

  where `corrections` is the number of corrections to attempt when finding a path with no obstacles.
-}
navigateToTarget :: Entity a => GameMap -> Float -> Bool -> Int -> Ship -> a -> String
navigateToTarget g v b i s e = do
    let d = distance s e
        a = angleRadians s e
        step = pi/90
    if (i < 0) then
        ""
    else
        if (entitiesBetween g b s e) then
            navigateToTarget g v b (i - 1) s (Location (x s + (cos (a + step)) * d) (y s + (sin (a + step)) * d))
        else
            thrust s (min d v) a

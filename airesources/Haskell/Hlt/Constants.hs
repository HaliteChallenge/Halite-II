-- | Various constant values.
module Hlt.Constants where

-- | Constant range in which a Ship can dock to a Planet.
dockRadius :: Float
dockRadius = 4.0

-- | Constant number of turns it takes to dock a Ship.
dockTurns :: Int
dockTurns = 5

-- | Constant radius of a Ship.
shipRadius :: Float
shipRadius = 0.5

-- | Constant maximum units a Ship can move in one frame.
maxSpeed :: Float
maxSpeed = 7.0

-- | Constant starting and max health of a Ship.
maxShipHealth :: Int
maxShipHealth = 255

-- | Constant Ship weapon cooldown period.
weaponCooldown :: Int
weaponCooldown = 1

-- | Constant Ship weapon radius.
weaponRadius :: Float
weaponRadius = 5.0

-- | Constant Ship weapon damage.
weaponDamage :: Int
weaponDamage = 64

-- | Constant Planet explosion radius.
explosionRadius :: Float
explosionRadius = 10.0

-- | Constant base productivity of a Planet.
baseProductivity :: Int
baseProductivity = 6

-- | Constant Ship spawn radius.
spawnRadius :: Float
spawnRadius = 2.0

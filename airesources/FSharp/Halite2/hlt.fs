module hlt

    open System;

    // #region util

    // converts degrees to radians
    let degreesToRadians (d: float) = d / float 180 * Math.PI;

    // converts radians to degrees
    let radiansToDegrees (r: float) = r / Math.PI * float 180;

    // #endregion

    // #region constants

    // Games will not have more players than this
    let MAX_PLAYERS = 4

    // Max number of units of distance a ship can travel in a turn
    let MAX_SPEED = 7

    // Radius of a ship
    let SHIP_RADIUS = 0.5

    // Starting health of ship, also its max
    let MAX_SHIP_HEALTH = 255

    // Starting health of ship, also its max
    let BASE_SHIP_HEALTH = 255

    // Weapon cooldown period
    let WEAPON_COOLDOWN = 1

    // Weapon damage radius
    let WEAPON_RADIUS = 5.0

    // Weapon damage
    let WEAPON_DAMAGE = 64

    // Radius in which explosions affect other entities
    let EXPLOSION_RADIUS = 10.0

    // Distance from the edge of the planet at which ships can try to dock
    let DOCK_RADIUS = 4.0

    // Number of turns it takes to dock a ship
    let DOCK_TURNS = 5

    // Number of production units per turn contributed by each docked ship
    let BASE_PRODUCTIVITY = 6

    // Distance from the planets edge at which new ships are created
    let SPAWN_RADIUS = 2.0

    // #endregion

    // #region entity

    // Unit in space
    type SpaceUnit = int

    // Radius
    type Radius = float

    // Health
    type Health = int

    // Velocity
    type Velocity = float

    // X coordinate
    type X = float

    // Y coordinate
    type Y = float

    // Position in 2D space
    type Position = { X: X; Y: Y; }

    // DockingStatus represents possible ship.DockingStatus values
    type DockingStatus = 
        | Undocked
        | Docking
        | Docked
        | Undocking

    // Entity captures spacial and ownership state for Planets and Ships
    type Entity = { 
        Id: int;
        OwnerId: int;
        X: X;
        Y: Y;
        Radius: Radius;
        Health: Health;
    }

    // Ship is a player controlled Entity made for the purpose of doing combat and mining Halite
    type Ship = {
        Entity: Entity;
        VelX: Velocity;
        VelY: Velocity;
        PlanetId: int;
        DockingStatus: DockingStatus;
        DockingProgress: SpaceUnit;
        WeaponCooldown: SpaceUnit;
    }

    // Planet object from which Halite is mined
    type Planet = {
        Entity: Entity;
        NumDockingSpots: int;
        NumDockedShips: int;
        CurrentProduction: int;
        RemainingResources: int;
        DockedShipIds: int[];
        DockedShips: Ship[] option;
        Owned: int;
    }

    // calculateDistanceTo returns a euclidean distance to the target
    let calculateDistanceTo entity target =
        let dX = target.X - entity.X;
        let dY = target.Y - entity.Y;

        Math.Sqrt(dX * dX + dY * dY);

    // calculateRadAngleTo returns an angle in radians to the target
    let calculateRadAngleTo entity target =
        let dX = target.X - entity.X;
        let dY = target.Y - entity.Y;

        Math.Atan2(dY, dX);

    // calculateAngleTo returns an angle in degrees to the target
    let calculateAngleTo entity target =
        calculateRadAngleTo entity target |> radiansToDegrees

    // closestPointTo returns the closest point that is at least minDistance from the target
    let closestPointTo entity target minDistance =
        let dist = calculateDistanceTo entity target - target.Radius - minDistance
        let angle = calculateRadAngleTo target entity
        let x = target.X + dist * Math.Cos angle
        let y = target.Y + dist * Math.Sin angle

        { X = x; Y = y; }

    // intToDockingStatus converts an int to a DockingStatus
    let intToDockingStatus i =
        match i with
            | 0 -> Undocked
            | 1 -> Docking
            | 2 -> Docked
            | 3 -> Undocking
            | _ -> Undocked
    
    // thrust generates a string describing the ship's intension to move during the current turn    
    let thrust (ship: Ship) (magnitude: float) angle =
        let boundedAngle = 
            match angle with
                | a when a > 0.0 -> Math.Floor(angle + 0.5) |> int
                | _ -> Math.Ceiling(angle - 0.5) |> int
            |> fun a -> ((a % 360) + 360) % 360            

        sprintf "t %s %s %s" (ship.Entity.Id |> string) (magnitude |> int |> string) (boundedAngle |> string)

    // dock generates a string describing the ship's intension to dock during the current turn
    let dock (ship: Ship) (planet: Planet) =
        sprintf "d %s %s" (ship.Entity.Id |> string) (planet.Entity.Id |> string)

    // undock generates a string describing the ship's intension to undock during the current turn
    let undock (ship: Ship) =
        sprintf "u %s %s" (ship.Entity.Id |> string)         

    // canDock indicates that a ship is close enough to a given planet to dock
    let canDock (ship: Ship) planet =
        let dist = calculateDistanceTo ship.Entity planet.Entity
        dist <= (ship.Entity.Radius + planet.Entity.Radius + float 4)

    // #endregion

    // #region gamemap

    type Player = {
        Id: int;
        Ships: Ship[];
    }

    type Map = {
        MyId: int;
        Width: int;
        Height: int;
        Planets: Planet[];
        Players: Player[];
        Entities: Entity[];
    }

    // obstaclesBetween demonstrates how the player might determine if the path
    // between two enitities is clear
    let obstaclesBetween gameMap startEntity endEntity =
        let x1 = startEntity.X
        let y1 = startEntity.Y
        let x2 = endEntity.X
        let y2 = endEntity.X

        let dx = x2 - x1
        let dy = y2 - y1
        let a = dx*dx + dy*dy + 1e-8
        let crossterms = x1*x1 - x1*x2 + y1*y1 - y1*y2

        gameMap.Entities
            |> Array.exists
                (fun entity -> 
                    match entity.Id with
                    | id when id = startEntity.Id || id = endEntity.Id -> false
                    | _ -> 
                    (
                        let closestDistance = calculateDistanceTo endEntity entity
                        
                        match closestDistance with
                        | d when d < entity.Radius + float 1 -> true
                        | _ ->
                        (
                            let x0 = entity.X
                            let y0 = entity.Y

                            let b = float -2 * (crossterms + x0*dx + y0*dy)
                            let t = -b / (float 2 * a)

                            match t with
                            | t when t <= float 0 || t >= float 1 -> false
                            | _ ->
                            (
                                let closestX = startEntity.X + dx*t
                                let closestY = startEntity.Y + dy*t
                                let closestDistance = Math.Sqrt(Math.Pow(closestX-x0, float 2) * +Math.Pow(closestY-y0, float 2))

                                match closestDistance with
                                | d when d <= entity.Radius + startEntity.Radius + float 1 -> true
                                | _ -> false
                            )
                        )
                    )
                )

    // nearestPlanetsByDistance orders all planets based on their proximity
    // to a given ship from nearest for farthest
    let nearestPlanetsByDistance gameMap (ship: Ship) =
        gameMap.Planets
        |> Array.sortBy (fun p -> calculateDistanceTo ship.Entity p.Entity)

    // #endregion

    // #region navigation

    // navigateBasic demonstrates how the player might move ships through space
    let navigateBasic (ship: Ship) target =
        let distance = calculateDistanceTo ship.Entity target
        let safeDistance = distance - ship.Entity.Radius - target.Radius - 0.1

        let angle = calculateAngleTo ship.Entity target
        let speed = 
            match distance with
                | d when d < 10.0 -> 3.0
                | _ -> 7.0
            |> fun s -> Math.Min(s, safeDistance)

        thrust ship speed angle   
    
    // Navigate demonstrates how the player might negotiate obstacles between
    // a ship and its target
    let navigate (ship: Ship) target gameMap =
        match obstaclesBetween gameMap ship.Entity target with
        | false -> navigateBasic ship target   
        | _ -> 
        (
            let x0 = Math.Min(ship.Entity.X, target.X)
            let x2 = Math.Max(ship.Entity.X, target.X)
            let y0 = Math.Min(ship.Entity.Y, target.Y)
            let y2 = Math.Max(ship.Entity.Y, target.Y)

            let dx = (x2 - x0) / 5.0
            let dy = (y2 - y0) / 5.0

            let mutable bestdist = 1000.0
            let mutable bestTarget = target

            let mutable x1 = x0
            while x1 <= x2 do 

                let mutable y1 = y0
                while y1 <= y2 do 

                    let intermediateTarget = {
                        Id = -1;
                        X = x1;
                        Y = y1;
                        OwnerId = 0;
                        Health = 0;
                        Radius = 0.0;
                    }
                    let ob1 = obstaclesBetween gameMap ship.Entity intermediateTarget
                    let ob2 = obstaclesBetween gameMap intermediateTarget target
                    let totdist = Math.Sqrt(Math.Pow(x1-x0, 2.0)+Math.Pow(y1-y0, 2.0)) + Math.Sqrt(Math.Pow(x1-x2, 2.0)+Math.Pow(y1-y2, 2.0))

                    if not ob1 && not ob2 && totdist < bestdist
                    then (
                        bestdist <- totdist
                        bestTarget <- intermediateTarget
                    )                    

                    y1 <- y1 + dy
                     
                x1 <- x1 + dx

            navigateBasic ship bestTarget
        )

    // #endregion

    // #region networking 1/2

    type Connection = {
        PlayerTag: int;
        Width: int;
        Height: int;
    }

    // #endregion

    // #region strategies

    // strategyBasicBot demonstrates how the player might direct their ships
    // in achieving victory
    let strategyBasicBot ship gameMap =
        let planets = nearestPlanetsByDistance gameMap ship

        let targetedPlanet =
            planets
            |> Array.tryFind (fun planet -> 
                (planet.Owned = 0 || planet.Entity.OwnerId = gameMap.MyId) && 
                planet.NumDockedShips < planet.NumDockingSpots && 
                planet.Entity.Id % 2 = ship.Entity.Id % 2
            )

        match targetedPlanet with
            | Some planet when canDock ship planet -> dock ship planet
            | Some planet -> 
                (
                    let closestPoint = closestPointTo ship.Entity planet.Entity 3.0
                    let closestEntity = { 
                        Id = -1; 
                        X = closestPoint.X;
                        Y = closestPoint.Y;
                        OwnerId = 0;
                        Health = 0;
                        Radius = 0.0;
                    }
                    navigate ship closestEntity gameMap
                )
            | None -> ""  

    // #endregion

    // #region parsing

    // parseShip from a slice of game state tokens
    let parseShip playerId (tokens: string[]) =
        let shipId = tokens.[0] |> int
        let shipX = tokens.[1] |> float
        let shipY = tokens.[2] |> float
        let shipHealth = tokens.[3] |> int
        let shipVelX = tokens.[4] |> float
        let shipVelY = tokens.[5] |> float
        let shipDockingStatus = tokens.[6] |> int
        let shipPlanetId = tokens.[7] |> int
        let shipDockingProgress = tokens.[8] |> int
        let shipWeaponCooldown = tokens.[9] |> int

        let shipEntity = {
            Id = shipId;
            X = shipX;
            Y = shipY;
            Radius = 0.5;
            Health = shipHealth;
            OwnerId = playerId;
        }

        let ship = {
            Entity = shipEntity;
            PlanetId = shipPlanetId;
            DockingStatus = intToDockingStatus shipDockingStatus;
            DockingProgress = shipDockingProgress;
            WeaponCooldown = shipWeaponCooldown;
            VelX = shipVelX;
            VelY = shipVelY;
        }

        (ship, tokens.[10..])
    
    // parsePlanet from a slice of game state tokens
    let parsePlanet (tokens: string[]) =
        let planetId = tokens.[0] |> int
        let planetX = tokens.[1] |> float
        let planetY = tokens.[2] |> float
        let planetHealth = tokens.[3] |> int
        let planetRadius = tokens.[4] |> float
        let planetNumDockingSpots = tokens.[5] |> int
        let planetCurrentProduction = tokens.[6] |> int
        let planetRemainingResources = tokens.[7] |> int
        let planetOwned = tokens.[8] |> int
        let planetOwnerId = tokens.[9] |> int
        let planetNumDockedShips = tokens.[10] |> int

        let planetEntity = {
            Id = planetId;
            X = planetX;
            Y = planetY;
            Radius = planetRadius;
            Health = planetHealth;
            OwnerId = planetOwnerId;
        }

        let dockedShipIds = 
            Array.init planetNumDockedShips
                (fun i -> tokens.[11 + i] |> int)

        let planet = {
            Entity = planetEntity;
            NumDockingSpots = planetNumDockingSpots;
            NumDockedShips = planetNumDockedShips;
            CurrentProduction = planetCurrentProduction;
            RemainingResources = planetRemainingResources;
            DockedShipIds = dockedShipIds;
            DockedShips = Option.None;
            Owned = planetOwned;
        }

        (planet, tokens.[(11 + planetNumDockedShips)..])

    // parsePlayer from a slice of game state tokens
    let parsePlayer (tokens: string[]) =
        let playerId = tokens.[0] |> int
        let playerNumShips = tokens.[1] |> float

        let playerNumShipsInt = playerNumShips |> int

        let mutable remainingTokens = tokens.[2..]
        let ships = 
            Array.init playerNumShipsInt
                (
                    fun i -> 
                        let ship, tokensnew = parseShip playerId remainingTokens
                        remainingTokens <- tokensnew
                        ship
                )         

        let player = {
            Id = playerId;
            Ships = ships;
        }

        (player, remainingTokens)

    // parseGameString from a slice of game state tokens
    let parseGameString connection (gameString: string) =
        let mutable tokens = gameString.Split(" ")
        let numPlayers = tokens.[0] |> int
        tokens <- tokens.[1..]

        let players = 
            Array.init numPlayers
                (fun _ -> 
                    let player, tokensnew = parsePlayer tokens
                    tokens <- tokensnew
                    player
                )

        let numPlanets = tokens.[0] |> int
        tokens <- tokens.[1..]

        let planets = 
            Array.init numPlanets
                (fun _ ->
                    let planet, tokensnew = parsePlanet tokens
                    tokens <- tokensnew
                    planet
                )

        let entities = 
            let shipEntities = 
                players
                |> Array.map (fun p -> p.Ships)
                |> Array.reduce Array.append
                |> Array.map (fun s -> s.Entity)
            let planetEntities = 
                planets
                |> Array.map (fun p -> p.Entity)

            Array.append shipEntities planetEntities

        {
            MyId = connection.PlayerTag;
            Width = connection.Width;
            Height = connection.Height;
            Planets = planets;
            Players = players;
            Entities = entities;
        }

    // #endregion

    // #region networking 2/2

    let sendString (input: string) = 
        Console.WriteLine input

    let getString () =
        Console.ReadLine().Trim()

    let getInt () =
        getString() |> int            

    // newConnection initializes a new connection for one of the bots
    // participating in a match
    let newConnection botName = 
        let playerTag = getInt()
        let sizeInfo = getString().Split " "
        let width = sizeInfo.[0] |> int
        let height = sizeInfo.[1] |> int

        sendString botName

        {
            PlayerTag = playerTag;
            Width = width;
            Height = height;
        }

    // updateMap decodes the current turn's game state from a string
    let updateMap connection =
        let gameString = getString()
        parseGameString connection gameString

    // submitCommands encodes the player's commands into a string
    let submitCommands (commandQueue: string[]) =
        let commandString = String.Join(" ", commandQueue)
        sendString commandString

    // #endregion
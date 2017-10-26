package hlt

import (
	"fmt"
	"math"
	"strconv"
)

type DockingStatus int

const (
	UNDOCKED DockingStatus = iota
	DOCKING
	DOCKED
	UNDOCKING
)

type Entity struct {
	X      float64
	Y      float64
	Radius float64
	Health float64
	Owner  int
	ID     int
}

type Position struct {
	X, Y float64
}

type Planet struct {
	Entity
	NumDockingSpots    float64
	NumDockedShips     float64
	CurrentProduction  float64
	RemainingResources float64
	DockedShipIDs      []int
	DockedShips        []Ship
	Owned              float64
	Distance           float64
}

type Ship struct {
	Entity
	VelX float64
	VelY float64

	PlanetID        int
	Planet          Planet
	DockingStatus   DockingStatus
	DockingProgress float64
	WeaponCooldown  float64
}

func (entity Entity) CalculateDistanceTo(target Entity) float64 {
	// returns euclidean distance to target
	dx := target.X - entity.X
	dy := target.Y - entity.Y

	return math.Sqrt(dx*dx + dy*dy)
}

func (entity Entity) CalculateAngleTo(target Entity) float64 {
	// returns angle in degrees from self to target
	return RadToDeg(entity.CalculateRadAngleTo(target))
}

func (entity Entity) CalculateRadAngleTo(target Entity) float64 {
	// returns angle in radians from self to target
	dx := target.X - entity.X
	dy := target.Y - entity.Y

	return math.Atan2(dy, dx)
}

func (entity Entity) ClosestPointTo(target Entity, minDistance float64) Entity {
	// returns closest point to entity that is at least minDistance from target
	dist := entity.CalculateDistanceTo(target) - target.Radius - minDistance
	angle := entity.CalculateRadAngleTo(target)
	x := entity.X + dist*math.Cos(angle)
	y := entity.Y + dist*math.Sin(angle)
	return Entity{
		X:      x,
		Y:      y,
		Radius: 0,
		Health: 0,
		Owner:  -1,
		ID:     -1,
	}
}

func ParseShip(playerID int, tokens []string) (Ship, []string) {
	shipID, _ := strconv.Atoi(tokens[0])
	shipX, _ := strconv.ParseFloat(tokens[1], 64)
	shipY, _ := strconv.ParseFloat(tokens[2], 64)
	shipHealth, _ := strconv.ParseFloat(tokens[3], 64)
	shipVelX, _ := strconv.ParseFloat(tokens[4], 64)
	shipVelY, _ := strconv.ParseFloat(tokens[5], 64)
	shipDockingStatus, _ := strconv.Atoi(tokens[6])
	shipPlanetID, _ := strconv.Atoi(tokens[7])
	shipDockingProgress, _ := strconv.ParseFloat(tokens[8], 64)
	shipWeaponCooldown, _ := strconv.ParseFloat(tokens[9], 64)

	shipEntity := Entity{
		X:      shipX,
		Y:      shipY,
		Radius: .5,
		Health: shipHealth,
		Owner:  playerID,
		ID:     shipID,
	}

	ship := Ship{
		PlanetID:        shipPlanetID,
		DockingStatus:   IntToDockingStatus(shipDockingStatus),
		DockingProgress: shipDockingProgress,
		WeaponCooldown:  shipWeaponCooldown,
		VelX:            shipVelX,
		VelY:            shipVelY,
		Entity:          shipEntity,
	}

	return ship, tokens[10:]
}

func ParsePlanet(tokens []string) (Planet, []string) {
	planetID, _ := strconv.Atoi(tokens[0])
	planetX, _ := strconv.ParseFloat(tokens[1], 64)
	planetY, _ := strconv.ParseFloat(tokens[2], 64)
	planetHealth, _ := strconv.ParseFloat(tokens[3], 64)
	planetRadius, _ := strconv.ParseFloat(tokens[4], 64)
	planetNumDockingSpots, _ := strconv.ParseFloat(tokens[5], 64)
	planetCurrentProduction, _ := strconv.ParseFloat(tokens[6], 64)
	planetRemainingResources, _ := strconv.ParseFloat(tokens[7], 64)
	planetOwned, _ := strconv.ParseFloat(tokens[8], 64)
	planetOwner, _ := strconv.Atoi(tokens[9])
	planetNumDockedShips, _ := strconv.ParseFloat(tokens[10], 64)

	planetEntity := Entity{
		X:      planetX,
		Y:      planetY,
		Radius: planetRadius,
		Health: planetHealth,
		Owner:  planetOwner,
		ID:     planetID,
	}

	planet := Planet{
		NumDockingSpots:    planetNumDockingSpots,
		NumDockedShips:     planetNumDockedShips,
		CurrentProduction:  planetCurrentProduction,
		RemainingResources: planetRemainingResources,
		DockedShipIDs:      nil,
		DockedShips:        nil,
		Owned:              planetOwned,
		Entity:             planetEntity,
	}

	for i := 0; i < int(planetNumDockedShips); i++ {
		dockedShipID, _ := strconv.Atoi(tokens[11+i])
		planet.DockedShipIDs = append(planet.DockedShipIDs, dockedShipID)
	}
	return planet, tokens[11+int(planetNumDockedShips):]
}

func IntToDockingStatus(i int) DockingStatus {
	statuses := [4]DockingStatus{UNDOCKED, DOCKING, DOCKED, UNDOCKING}
	return statuses[i]
}

func (ship Ship) Thrust(magnitude float64, angle float64) string {
	return fmt.Sprintf("t %s %s %s", strconv.Itoa(ship.ID), strconv.Itoa(int(magnitude)), strconv.Itoa(int(angle)))
}

func (ship Ship) Dock(planet Planet) string {
	return fmt.Sprintf("d %s %s", strconv.Itoa(ship.ID), strconv.Itoa(planet.ID))
}

func (ship Ship) Undock() string {
	return fmt.Sprintf("u %s", strconv.Itoa(ship.ID))
}

func (ship Ship) NavigateBasic(target Entity, gameMap Map) string {
	distance := ship.CalculateDistanceTo(target)

	angle := ship.CalculateAngleTo(target)
	speed := 7.0
	if distance < 10 {
		speed = 3.0
	}

	speed = math.Min(speed, distance)
	return ship.Thrust(speed, angle)
}

func (ship Ship) CanDock(planet Planet) bool {
	dist := ship.CalculateDistanceTo(planet.Entity)

	return dist <= (planet.Radius + 6)
}

func (ship Ship) Navigate(target Entity, gameMap Map) string {
	ob := gameMap.ObstaclesBetween(ship.Entity, target)

	if !ob {
		return ship.NavigateBasic(target, gameMap)
	}

	x0 := math.Min(ship.X, target.X)
	x2 := math.Max(ship.X, target.X)
	y0 := math.Min(ship.Y, target.Y)
	y2 := math.Max(ship.Y, target.Y)

	dx := (x2 - x0) / 5
	dy := (y2 - y0) / 5
	bestdist := 1000.0
	bestTarget := target

	for x1 := x0; x1 <= x2; x1 += dx {
		for y1 := y0; y1 <= y2; y1 += dy {
			intermediateTarget := Entity{
				X:      x1,
				Y:      y1,
				Radius: 0,
				Health: 0,
				Owner:  0,
				ID:     -1,
			}
			ob1 := gameMap.ObstaclesBetween(ship.Entity, intermediateTarget)
			if !ob1 {
				ob2 := gameMap.ObstaclesBetween(intermediateTarget, target)
				if !ob2 {
					totdist := math.Sqrt(math.Pow(x1-x0, 2)+math.Pow(y1-y0, 2)) + math.Sqrt(math.Pow(x1-x2, 2)+math.Pow(y1-y2, 2))
					if totdist < bestdist {
						bestdist = totdist
						bestTarget = intermediateTarget

					}
				}
			}
		}
	}

	return ship.NavigateBasic(bestTarget, gameMap)
}

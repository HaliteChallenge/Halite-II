package hlt

// StrategyBasicBot demonstrates how the player might direct their ships
// in achieving victory
func StrategyBasicBot(ship Ship, gameMap Map) string {
	planets := gameMap.NearestPlanetsByDistance(ship)

	for i := 0; i < len(planets); i++ {
		planet := planets[i]
		if (planet.Owned == 0 || planet.Owner == gameMap.MyID) && planet.NumDockedShips < planet.NumDockingSpots && planet.ID%2 == ship.ID%2 {
			if ship.CanDock(planet) {
				return ship.Dock(planet)
			}
			return ship.Navigate(ship.ClosestPointTo(planet.Entity, 3), gameMap)
		}
	}

	return ""
}

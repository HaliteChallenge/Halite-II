package hlt

func StrategyBasicBot(ship Ship, gameMap Map) string {
	planets := gameMap.NearestPlanetsByDistance(ship)

	for i := 0; i < len(planets); i++ {
		planet := planets[i]
		if (planet.Owned == 0 || planet.Owner == gameMap.MyId) && planet.NumDockedShips < planet.NumDockingSpots && planet.Id%2 == ship.Id%2 {
			if ship.CanDock(planet) {
				return ship.Dock(planet)
			} else {

				return ship.Navigate(ship.ClosestPointTo(planet.Entity, 3), gameMap)
			}
		}
	}

	return ""
}

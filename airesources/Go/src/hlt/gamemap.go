package hlt

import (
	"strconv"
	"strings"
	"math"
	"sort"

)

type Map struct {
	MyId, Width, Height int
	Planets             [] Planet /// preallocating for speed, assuming we cant have > 100 planets
	Players             [4] Player
	Entities            []Entity
}

type Player struct {
	Id    int
	Ships [] Ship /// preallocating for speed, assuming we cant have > 10k ships.
}

func ParsePlayer(tokens []string) (Player, [] string) {
	playerId, _ := strconv.Atoi(tokens[0])
	playerNumShips, _ := strconv.ParseFloat(tokens[1], 64)

	player := Player{
		Id:    playerId,
		Ships: []Ship{},
	}

	tokens = tokens[2:]
	for i := 0; float64(i) < playerNumShips; i++ {
		ship, tokensnew := ParseShip(playerId, tokens)
		tokens = tokensnew
		player.Ships = append(player.Ships, ship)
	}

	return player, tokens
}

func ParseGameString(gameString string, self Map) Map {
	tokens := strings.Split(gameString, " ")
	numPlayers, _ := strconv.Atoi(tokens[0])
	tokens = tokens[1:]

	for i := 0; i < numPlayers; i++ {
		player, tokensnew := ParsePlayer(tokens)
		tokens = tokensnew
		self.Players[player.Id] = player
		for j := 0; j < len(player.Ships); j++ {
			self.Entities = append(self.Entities, player.Ships[j].Entity)
		}
	}

	numPlanets, _ := strconv.Atoi(tokens[0])
	tokens = tokens[1:]

	for i := 0; i < numPlanets; i++ {
		planet, tokensnew := ParsePlanet(tokens)
		tokens = tokensnew
		self.Planets = append(self.Planets, planet)
		self.Entities = append(self.Entities, planet.Entity)
	}

	return self
}

func (gameMap Map) ObstaclesBetween(start Entity, end Entity) bool {

	x1 := start.X
	y1 := start.Y
	x2 := end.X
	y2 := end.Y
	dx := x2 - x1
	dy := y2 - y1
	a := dx*dx + dy*dy + 1e-8
	crossterms := x1*x1 - x1*x2 + y1*y1 - y1*y2

	for i := 0; i < len(gameMap.Entities); i++ {
		entity := gameMap.Entities[i]
		if entity.Id == start.Id || entity.Id == end.Id {
			continue
		}

		x0 := entity.X
		y0 := entity.Y

		closest_distance := end.CalculateDistanceTo(entity)
		if closest_distance < entity.Radius+1 {
			return true
		}

		b := -2 * (crossterms + x0*dx + y0*dy)
		t := -b / (2 * a)

		if t <= 0 || t >= 1 {
			continue
		}

		closest_x := start.X + dx*t
		closest_y := start.Y + dy*t
		closest_distance = math.Sqrt(math.Pow(closest_x-x0, 2) * + math.Pow(closest_y-y0, 2))

		if closest_distance <= entity.Radius+start.Radius+1 {
			return true
		}
	}
	return false
}
func (gameMap Map) NearestPlanetsByDistance(ship Ship) [] Planet {
	planets := gameMap.Planets

	for i := 0; i < len(planets); i++ {

		planets[i].Distance = ship.CalculateDistanceTo(planets[i].Entity)
	}

	sort.Sort(byDist(planets))


	return planets
}

type byDist [] Planet

func (a byDist) Len() int           { return len(a) }
func (a byDist) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byDist) Less(i, j int) bool { return a[i].Distance < a[j].Distance }

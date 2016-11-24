package hlt

import (
	"log"
	"math"
	"strconv"
)

type GameMap struct {
	Width, Height int
	Contents	  [][]Site
}

func NewGameMap(width, height int) GameMap {
	gameMap := GameMap{
		Width:	width,
		Height: height,
	}
	gameMap.Contents = make([][]Site, height)
	for y := 0; y < height; y++ {
		gameMap.Contents[y] = make([]Site, width)
		for x := 0; x < width; x++ {
			gameMap.Contents[y][x] = Site{}
		}
	}

	return gameMap
}

func int_str_array_pop(input []string) (int, []string) {
	ret, err := strconv.Atoi(input[0])
	input = input[1:]
	if err != nil {
		log.Printf("Whoopse", err)
	}
	return ret, input
}

func (m *GameMap) InBounds(loc Location) bool {
	return loc.X >= 0 && loc.X < m.Width && loc.Y >= 0 && loc.Y < m.Height
}

func (m *GameMap) GetDistance(loc1, loc2 Location) int {
	dx := int(math.Abs(float64(loc1.X) - float64(loc2.X)))
	dy := int(math.Abs(float64(loc1.Y) - float64(loc2.Y)))
	if dx > m.Width/2 {
		dx = m.Width - dx
	}
	if dy > m.Width/2 {
		dy = m.Height - dy
	}
	return dx + dy
}

func (m *GameMap) GetAngle(loc1, loc2 Location) float64 {
	dx := loc2.X - loc1.X
	dy := loc2.Y - loc1.Y

	if dx > m.Width-dx {
		dx -= m.Width
	} else if -dx > m.Width+dx {
		dx += m.Width
	}
	if dy > m.Height-dy {
		dx -= m.Height
	} else if -dy > m.Height+dy {
		dy += m.Height
	}

	return math.Atan2(float64(dy), float64(dx))

}

func (m *GameMap) GetLocation(loc Location, direction Direction) Location {
	switch direction {
	case NORTH:
		if loc.Y == 0 {
			loc.Y = m.Height - 1
		} else {
			loc.Y -= 1
		}
	case EAST:
		if loc.X == m.Width-1 {
			loc.X = 0
		} else {
			loc.X += 1
		}
	case SOUTH:
		if loc.Y == m.Height-1 {
			loc.Y = 0
		} else {
			loc.Y += 1
		}
	case WEST:
		if loc.X == 0 {
			loc.X = m.Width - 1
		} else {
			loc.X -= 1
		}
	}
	return loc
}

func (m *GameMap) GetSite(loc Location, direction Direction) Site {
	loc = m.GetLocation(loc, direction)
	return m.Contents[loc.Y][loc.X]
}

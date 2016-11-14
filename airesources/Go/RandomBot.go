package main

import (
	"hlt"

	"math/rand"
)

func main () {
	conn, gameMap := hlt.NewConnection("RandomBot")
	for {
		var moves hlt.MoveSet
		gameMap = conn.GetFrame()
		for y := 0; y < gameMap.Height; y++ {
			for x := 0; x < gameMap.Width; x++ {
				loc := hlt.NewLocation(x,y)
				if gameMap.GetSite(loc, hlt.STILL).Owner == conn.PlayerTag {
					moves = append(moves, hlt.Move {
						Location: loc,
						Direction: hlt.Direction(rand.Int() % 5),

					})
				}
			}
		}
		conn.SendFrame(moves)

	}
}

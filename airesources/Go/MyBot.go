package main

import (
	"fmt"
	"./src/hlt"
	"log"
	"os"
	"strconv"
)
// golang starter kit with logging and basic pathfinding
// Arjun Viswanathan 2017 / github arjunvis

func main() {
	logging := true
	botName := "GoBot"

	conn := hlt.NewConnection(botName)

	// set up logging
	if logging {
		fname := strconv.Itoa(conn.PlayerTag) + "_gamelog.log"
		f, err := os.OpenFile(fname, os.O_RDWR|os.O_CREATE|os.O_APPEND, 0666)
		if err != nil {
			fmt.Println("error opening file: %v", err)
		}
		defer f.Close()
		log.SetOutput(f)
	}
	gameMap := conn.UpdateMap()
	gameturn := 1
	for true {
		gameMap = conn.UpdateMap()
		commandQueue := [] string{}

		myPlayer := gameMap.Players[gameMap.MyId]
		myShips := myPlayer.Ships

		for i := 0; i < len(myShips); i++ {
			ship := myShips[i]
			if ship.DockingStatus == hlt.UNDOCKED {
				commandQueue = append(commandQueue, hlt.StrategyBasicBot(ship, gameMap))
			}
		}
		log.Printf("Turn %v\n",gameturn)
		conn.SubmitCommands(commandQueue)
		gameturn++
	}
}

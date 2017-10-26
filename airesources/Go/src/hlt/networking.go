package hlt

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

// Connection performs all of the IO operations required to communicate
// game state and player movements with the Halite engine
type Connection struct {
	width, height int
	PlayerTag     int
	reader        *bufio.Reader
	writer        io.Writer
}

func (c *Connection) sendString(input string) {
	fmt.Println(input)
}

func (c *Connection) getString() string {
	retstr, _ := c.reader.ReadString('\n')
	retstr = strings.TrimSpace(retstr)
	return retstr
}

func (c *Connection) getInt() int {
	i, err := strconv.Atoi(c.getString())
	if err != nil {
		log.Printf("Errored on initial tag: %v", err)
	}
	return i
}

// NewConnection initializes a new connection for one of the bots
// participating in a match
func NewConnection(botName string) Connection {
	conn := Connection{
		reader: bufio.NewReader(os.Stdin),
		writer: os.Stdout,
	}
	conn.PlayerTag = conn.getInt()
	sizeInfo := strings.Split(conn.getString(), " ")
	width, _ := strconv.Atoi(sizeInfo[0])
	height, _ := strconv.Atoi(sizeInfo[1])
	conn.width = width
	conn.height = height
	conn.sendString(botName)
	return conn
}

// UpdateMap decodes the current turn's game state from a string
func (c *Connection) UpdateMap() Map {
	log.Printf("--- NEW TURN --- \n")
	gameString := c.getString()
	gameMap := ParseGameString(c, gameString)
	log.Printf("    Parsed map")
	return gameMap
}

// SubmitCommands encodes the player's commands into a string
func (c *Connection) SubmitCommands(commandQueue []string) {
	commandString := strings.Join(commandQueue, " ")
	log.Printf("Final string : %+v\n", commandString)
	c.sendString(commandString)
}

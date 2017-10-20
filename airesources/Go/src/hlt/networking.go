package hlt

import (
	"bufio"
	"io"
	"fmt"
	"log"
	"strings"
	"os"
	"strconv"
)

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
		log.Printf("Errored on initial tag: ", err)
	}
	return i
}

func NewConnection(botName string) (Connection) {
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

func (c *Connection) UpdateMap() Map {
	log.Printf("--- NEW TURN --- \n")
	gameString := c.getString()

	gameMap := Map{
		MyId:     c.PlayerTag,
		Width:    c.width,
		Height:   c.height,
		Planets:  []Planet{},
		Players:  [4]Player{},
		Entities: []Entity{},
	}
	//log.Printf("%+v\n",gameMap)
	gameMap = ParseGameString(gameString, gameMap)
	log.Printf("    Parsed map")
	return gameMap
}

func (c *Connection) SubmitCommands(commandQueue [] string) {
	commandString := strings.Join(commandQueue, " ")
	log.Printf("Final string : %+v\n", commandString)
	c.sendString(commandString)
}

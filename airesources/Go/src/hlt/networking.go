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

type Direction int

const (
	STILL Direction = iota
	NORTH
	EAST
	SOUTH
	WEST
)

var Directions = []Direction{STILL, NORTH,EAST, SOUTH, WEST}
var CARDINALS = []Direction{NORTH,EAST, SOUTH, WEST}


type Site struct {
	Owner	   int
	Strength   int
	Production int
}

type Location struct {
	Y, X int
}

func NewLocation(x, y int) Location {
	return Location{
		X: x,
		Y: y,
	}
}

type Move struct {
	Location  Location
	Direction Direction
}

type MoveSet []Move

func (ms MoveSet) serialize() string {
	var retstr string
	for _, move := range ms {
		retstr = fmt.Sprintf("%s %d %d %d", retstr, move.Location.X, move.Location.Y, move.Direction)
	}
	return retstr
}

type Connection struct {
	width, height int
	PlayerTag	  int
	productions   [][]int
	reader		  *bufio.Reader
	writer		  io.Writer
}

func (c *Connection) deserializeMap() GameMap {
	splitString := strings.Split(c.getString(), " ")

	m := NewGameMap(c.width, c.height)

	var x, y, owner, counter int
	for y != m.Height {
		counter, splitString = int_str_array_pop(splitString)
		owner, splitString = int_str_array_pop(splitString)
		for a := 0; a < counter; a++ {
			m.Contents[y][x].Owner = owner

			x += 1
			if x == m.Width {
				x = 0
				y += 1
			}
		}
	}

	for y := 0; y < m.Height; y++ {
		for x := 0; x < m.Width; x++ {
			m.Contents[y][x].Strength, splitString = int_str_array_pop(splitString)
			m.Contents[y][x].Production = c.productions[y][x]
		}
	}

	return m
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
		log.Printf("Whoopse", err)
	}
	return i
}

func (c *Connection) deserializeMapSize() {
	splitString := strings.Split(c.getString(), " ")
	c.width, splitString = int_str_array_pop(splitString)
	c.height, splitString = int_str_array_pop(splitString)
}

func (c *Connection) deserializeProductions() {
	splitString := strings.Split(c.getString(), " ")

	c.productions = make([][]int, c.height)
	for y := 0; y < c.height; y++ {
		c.productions[y] = make([]int, c.width)
		for x := 0; x < c.width; x++ {
			c.productions[y][x], splitString = int_str_array_pop(splitString)
		}
	}
}

func NewConnection(name string) (Connection, GameMap) {
	conn := Connection{
		reader: bufio.NewReader(os.Stdin),
		writer: os.Stdout,
	}
	conn.PlayerTag = conn.getInt()
	conn.deserializeMapSize()
	conn.deserializeProductions()
	conn.sendString(name)

	return conn, conn.deserializeMap()
}

func (c *Connection) GetFrame() GameMap {
	return c.deserializeMap()
}

func (c *Connection) SendFrame(moves MoveSet) {
	c.sendString(moves.serialize())
}

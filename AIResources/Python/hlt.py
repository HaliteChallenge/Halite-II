from collections import namedtuple
import random
import math

Location = namedtuple("Location", "x y")
Site = namedtuple("Site", "owner strength")
Move = namedtuple("Move", "loc dir")

class Map:
	def __init__(width = 0, height = 0, numberOfPlayers = 0):
		self.map_width = width
		self.map_height = height
		self.contents = []

		for y in range(0, map_height):
			row = []
			for x in range(0, map_width):
				row.append(Site(0, 0))

		## Place players onto map
		takenSpots = []
		minDistance = math.sqrt(map_height*map_width) / 2
		for a in range(1, numberOfPlayers + 1):
			bad = True
			counter = 0
			location = None
			while bad == True:
				bad == False
				location = Location(int(random.random()*map_width), int(random.random()*map_height))
				for spot in takenSpots:
					if getDistance(location, spot) <= minDistance:
						bad = True
						break
				counter += 1
				if counter > 150:
					counter = 0
					minDistance *= 0.85
			contents[location.x][location.y] = Site(a, 255)
			takenSpots.append(location)
	def inBounds(l):
		return l.x < map_width and l.y < map_height

	def getDistance(l1, l2):
		dx = math.abs(l1.x - l2.x)
		dy = math.abs(l1.y - l2.y)
		if dx > map_width / 2:
			dx = map_width - dx
		if dy > map_height / 2:
			dy = map_height - dy
		return math.sqrt((dx*dx) + (dy*dy))

	def getAngle(l1, l2):
		dx = l2.x - l1.x
		dy = l2.y - l1.y

		if dx > map_width - dx:
			dx -= map_width
		elif -dx > map_width + dx:
			dx += map_width

		if dy > map_height - dy:
			dy -= map_height
		elif -dy > map_height + dy:
			dy += map_height
		return math.atan2(dy, dx)

	def getLocation(l, direction):
		if direction != STILL:
			if direction == NORTH:
				if l.y == 0:
					l.y = map_height - 1
				else:
					l.y -= 1
			elif direction == EAST:
				if l.x == map_width - 1:
					l.x = 0
				else:
					l.x += 1
			elif direction == SOUTH:
				if l.y == map_height - 1:
					l.y = 0
				else:
					l.y += 1
			elif direction == WEST:
				if l.x == 0:
					l.x = map_width - 1
				else:
					l.x -= 1
		return l
	def getSite(l, direction):
		l = getLocation(l, direction)
		return contents[l.y][l.x]

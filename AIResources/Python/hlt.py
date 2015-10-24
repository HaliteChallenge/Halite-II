import random
import math

STILL = 0
NORTH = 1
EAST = 2
SOUTH = 3
WEST = 4


class Location:
	def __init__(self, x=0, y=0):
		self.x = x
		self.y = y
class Site:
	def __init__(self, owner=0, strength=0):
		self.owner = owner
		self.strength = strength
class Move:
	def __init__(self, loc=0, direction=0):
		self.loc = loc
		self.direction = direction

class Map:
	def __init__(self, width = 0, height = 0, numberOfPlayers = 0):
		self.map_width = width
		self.map_height = height
		self.contents = []

		for y in range(0, self.map_height):
			row = []
			for x in range(0, self.map_width):
				row.append(Site(0, 0))
			self.contents.append(row)

		## Place players onto map
		takenSpots = []
		minDistance = math.sqrt(self.map_height*self.map_width) / 2
		for a in range(1, numberOfPlayers + 1):
			bad = True
			counter = 0
			location = None
			while bad == True:
				bad == False
				location = Location(int(random.random()*self.map_width), int(random.random()*self.map_height))
				for spot in takenSpots:
					if getDistance(location, spot) <= minDistance:
						bad = True
						break
				counter += 1
				if counter > 150:
					counter = 0
					minDistance *= 0.85
			self.contents[location.x][location.y] = Site(a, 255)
			takenSpots.append(location)
	def inBounds(l):
		return l.x < self.map_width and l.y < self.map_height

	def getDistance(l1, l2):
		dx = math.abs(l1.x - l2.x)
		dy = math.abs(l1.y - l2.y)
		if dx > self.map_width / 2:
			dx = self.map_width - dx
		if dy > self.map_height / 2:
			dy = self.map_height - dy
		return math.sqrt((dx*dx) + (dy*dy))

	def getAngle(l1, l2):
		dx = l2.x - l1.x
		dy = l2.y - l1.y

		if dx > self.map_width - dx:
			dx -= self.map_width
		elif -dx > self.map_width + dx:
			dx += self.map_width

		if dy > self.map_height - dy:
			dy -= self.map_height
		elif -dy > self.map_height + dy:
			dy += self.map_height
		return math.atan2(dy, dx)

	def getLocation(l, direction):
		if direction != STILL:
			if direction == NORTH:
				if l.y == 0:
					l.y = self.map_height - 1
				else:
					l.y -= 1
			elif direction == EAST:
				if l.x == self.map_width - 1:
					l.x = 0
				else:
					l.x += 1
			elif direction == SOUTH:
				if l.y == self.map_height - 1:
					l.y = 0
				else:
					l.y += 1
			elif direction == WEST:
				if l.x == 0:
					l.x = self.map_width - 1
				else:
					l.x -= 1
		return l
	def getSite(l, direction):
		l = getLocation(l, direction)
		return self.contents[l.y][l.x]
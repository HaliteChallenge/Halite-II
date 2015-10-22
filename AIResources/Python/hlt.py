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
				location = Location(int(random.random()*map_width), int(random.random()*map_height));
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
	def getDistance(l1, l2):
		dx = math.abs(l1.x - l2.x)
		dy = math.abs(l1.y - l2.y);
		if dx > map_width / 2:
			dx = map_width - dx;
		if dy > map_height / 2:
			dy = map_height - dy;
		return math.sqrt((dx*dx) + (dy*dy));
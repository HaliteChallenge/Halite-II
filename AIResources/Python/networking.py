import socket

def serializeMoveSet(moves):
	returnString = ""
    for move in moves:
    	returnString += a->loc.x + " " + a->loc.y + " " + a->dir + " " 
	return returnString

def deserializeMap(inputString):
	
	splitString = split(inputString)
   	width = splitString.pop(0)
   	height = splitString.pop(0)

   	m = Map(width, height)
    
    y = 0
    x = 0
    counter = 0
    owner = 0
    while y != m.m_height:
    	counter = splitString.pop(0)
    	owner = splitString.pop(0)
        for a in range(0, counter):
            m.contents[y][x].owner = owner
            x += 1
            if x == m.m_width:
            	x = 0
                y += 1

    for a in range(0, len(m.contents)): 
        for b in range(0, len(m.contents[a])):
            m.contents[a][b].strength = splitString.pop(0)
            
	return m
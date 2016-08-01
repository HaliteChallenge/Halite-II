dimensions = [(1, 20), (2, 25), (3, 30), (4, 35), (3, 40), (2, 45), (1, 50)]
averagePlayersPerGame = 4
playersPerServers = 6
perServerCost = 5
averagePlayers = 5000
totalTime = 0
totalMatches = 0
for dimension in dimensions:
    totalMatches += dimension[0]
    totalTime += dimension[0] * (15000 + pow(dimension[1], 3)*10/3)


timePerPlayerPerMatch = totalTime*averagePlayers/(averagePlayersPerGame*(averagePlayers/playersPerServers)*totalMatches)
print("Time per match per player in minutes: " + str(timePerPlayerPerMatch/(60*1000)))
costPerPlayer = perServerCost/playersPerServers
print("Cost per player: " + str(costPerPlayer))
totalCost = perServerCost*(averagePlayers/playersPerServers)
print("Total cost: " + str(totalCost))

include("hlt.jl")
include("networking.jl")

myID, gameMap = getInit()
sendInit("RandomJuliaBot")

while true
  moves = Vector{Move}()
  gameMap = getFrame()
  for y in 1:gameMap.height
    for x in 1:gameMap.width
      if getSite(gameMap, Location(x, y)).owner == myID
        push!(moves, Move(Location(x, y), rand(0:4)))
      end
    end
  end
  sendFrame(moves)
end

const STILL = 0
const NORTH = 1
const EAST = 2
const SOUTH = 3
const WEST = 4

const DIRECTIONS = [a for a in 0:4]
const CARDINALS = [a for a in 1:4]

const ATTACK = 0
const STOP_ATTACK = 1

type Location
  x :: Int64
  y :: Int64
  Location(x::Int64, y::Int64) = new(x, y)
  Location() = Location(0, 0)
end

type Site
  owner :: Int64
  strength :: Int64
  production :: Int64
  Site(owner::Int64, strength::Int64, production::Int64) = new(owner, strength, production)
  Site() = Site(0, 0, 0)
end

type Move
  loc :: Location
  direction :: Int64
  Move(loc::Location, direction::Int64) = new(loc, direction)
  Move() = Move(Location(), 0)
end

type GameMap
  width :: Int64
  height :: Int64
  contents :: Vector{Vector{Site}}
  function GameMap(width::Int64, height::Int64)
    contents = Vector{Vector{Site}}()
    for y in 0:height-1
      row = Vector{Site}()
      for x in 0:width-1
        push!(row, Site(0, 0, 0))
      end
      push!(contents, row)
    end
    new(width, height, contents)
  end
  GameMap() = GameMap(0, 0)
end

isBounds(gm::GameMap, l::Location) = l.x >= 0 && l.x < gm.width && l.y >= 0 && l.y < gm.height

function getDistance(gm::GameMap, l1::Location, l2::Location)
  dx = abs(l1.x - l2.x)
  dy = abs(l1.y - l2.y)
  if dx > gm.width / 2
    dx = gm.width - dx
  end
  if dy > gm.height / 2
    dy = gm.height - dy
  end
  dx + dy
end

function getAngle(gm::GameMap, l1::Location, l2::Location)
  dx = l2.x - l1.x
  dy = l2.y - l1.y

  if dx > gm.width - dx
    dx -= gm.width
  elseif -dx > gm.width + dx
    dx += gm.width
  end

  if dy > gm.height - dy
    dy -= gm.height
  elseif -dy > gm.height + dy
    dy += gm.height
  end
  atan2(dy, dx)
end

function getLocation(gm::GameMap, loc::Location, direction::Int64)
  l = deepcopy(loc)
  if direction != STILL
    if direction == NORTH
      if l.y == 0
        l.y = gm.height - 1
      else
        l.y = l.y - 1
      end
    elseif direction == EAST
      if l.x == gm.width - 1
        l.x = 0
      else
        l.x = l.x + 1
      end
    elseif direction == SOUTH
      if l.y == gm.height - 1
        l.y = 0
      else
        l.y = l.y + 1
      end
    elseif direction == WEST
      if l.x == 0
        l.x = gm.width - 1
      else
        l.x = l.x - 1
      end
    end
  end
  l
end

function getSite(gm::GameMap, l::Location, direction::Int64=STILL)
  l = getLocation(gm, l, direction)
  gm.contents[l.y + 1][l.x + 1]
end

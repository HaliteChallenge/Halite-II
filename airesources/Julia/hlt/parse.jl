function parse_map!()
    id = parse(Int, readline())
    width, height = parse.(Int, split(readline()))
    return GameMap(id, width, height, parse_turn!()...)
end
update_map(game::Game) = GameMap(game.id, game.width, game.height, parse_turn!()...)
start_game(game::Game) = print(game.botname, "\n")

"""
    parse_turn(parse_turn(turn::Vector{String}))
Parse a turn message received from halite.
See the [starter-kit-reference][1].
[1]: https://halite.io/learn-programming-challenge/downloads-and-starter-kits/create-new-starter-kit
"""
function parse_turn!(turn::Vector{SubString{String}})
    players = Dict([parse_player!(turn) for _ in 1:parse(Int,shift!(turn))])
    planets = Dict([parse_planet!(turn) for _ in 1:parse(Int,shift!(turn))])
    @assert isempty(turn)
    return players, planets
end
parse_turn!() = parse_turn!(split(readline()))

"""
    parse_player!(tokens)
Parse a vector of `1+n` tokens as a `Player` where `n` is the number of owned ships.
Since constructors fall back to `convert` methods, parse all strings to `Float64` or `Int`.
"""
function parse_player!(tokens)
    id = parse(Int, shift!(tokens))
    nr_ships = parse(Int,shift!(tokens))
    ships = Dict([parse_ships!(id, tokens) for _ in 1:nr_ships])
    return id => Player(id, ships)
end

function parse_ships!(owner_id, tokens) 
    id = parse(Int, shift!(tokens))
    id => Ship(owner_id, id, parse.(Float64, splice!(tokens, 1:9))...)
end

"""
    parse_planet!(tokens)
Parse a vector of `12+n` tokens as a `Planet` where `n` is the number of docked ships.
Since constructors fall back to `convert` methods, parse all strings to `Float64` or `Int`.
"""
function parse_planet!(tokens)
    id = parse(Int, shift!(tokens))
    parameters = parse.(Float64,splice!(tokens, 1:9))
    ship_ids = parse.(Int,splice!(tokens, 1:parse(Int,shift!(tokens))))
    return id => Planet(id, parameters..., ship_ids)
end

# Constructors ommitting deprecated parameters
Ship(owner_id, id, x, y, hp, xv, yv, docked, docked_planet, progress, cooldown) = 
Ship(owner_id, id, x, y, hp,         docked, docked_planet, progress, cooldown)

Planet(id, x, y, hp, radius, docks, current_prod, rm_prod,  owned, owner_id, docked_ships_ids) = 
Planet(id, x, y, hp, radius, docks, current_prod,           owned, owner_id, docked_ships_ids)
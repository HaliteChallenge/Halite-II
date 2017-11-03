struct Player
    id::String
    ships::Dict{String, Ship}

    function Player(id::String)
        new(id, Dict{String, Ship}())
    end
end

"""
    get_ship(player, ship_id::String)

Returns the ship designated by `ship_id` belonging to this user.
"""
get_ship(player::Player, ship_id::String) = player.ships[ship_id]

"""
    all_ships(player, collect = false)

    Returns either iterator over all ships which belong to the user (collect = false), or a list of all ships which belong to the user (collect = true).
"""
function all_ships(player::Player, coll::Bool=false)
    if coll
        return collect(values(player.ships))
    else
        return values(player.ships)
    end
end

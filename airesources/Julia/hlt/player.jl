struct Player
    id::String
    ships::Dict{String, Ship}
end
Player(id::String) = Player(id, Dict{String, Ship}())

"""
    get_ship(player, ship_id::String)

Returns the ship designated by `ship_id` belonging to this user.
"""
get_ship(player::Player, ship_id::String) = player.ships[ship_id]

"""
    all_ships(player)

Returns iterator over all ships which belong to the user.
"""
all_ships(player::Player) = values(player.ships)

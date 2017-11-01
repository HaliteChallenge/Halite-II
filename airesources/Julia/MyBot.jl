### Welcome to your first Halite-II bot!
### 
### This bot's name is Settler. It's purpose is simple (don't expect it to win complex games :) ):
### 1. Initialize game
### 2. If a ship is not docked and there are unowned planets
### 2.a. Try to Dock in the planet if close enough
### 2.b If not, go towards the planet
### 
### Note: Please do not place print statements here as they are used to communicate with the Halite engine. If you need
### to log anything use the logging module.

# Let's start by importing the Halite Starter Kit so we can interface with the Halite engine
include("hlt/halite.jl")
using Halite

using Memento
logger = get_logger("bot-logger")

game = Game("Settler")

# Here one can do all preliminary checks and warmups, using initial 60s timeout.
turn = 1

start_game(game)

while true
    # TURN START
    # Update the map for the new turn and get the latest version
    debug(logger, @sprintf("------ TURN %s ------", turn))
    game_map = update_map(game)
    
    # Here we define the set of commands to be sent to the Halite engine at the end of the turn
    command_queue = Vector{String}()
    # For every ship that I control
    for ship in all_ships(get_me(game_map))
        if isdocked(ship)
            # skip this ship
            continue
        end

        # For each planet in the game (only non-destroyed planets are included)
        for planet in all_planets(game_map)
            if isowned(planet)
                # Skip this planet
                continue
            end

            # If we can dock, let's (try to) dock. If two ships try to dock at once, neither will be able to.
            if can_dock(ship, planet)
                # We add the command by appending it to the command_queue
                push!(command_queue, dock(ship, planet))
            else
                # If we can't dock, we move towards the closest empty point near this planet (by using closest_point_to)
                # with constant speed. Don't worry about pathfinding for now, as the command will do it for you.
                # We run this navigate command each turn until we arrive to get the latest move.
                # Here we move at half our maximum speed to better control the ships
                # In order to execute faster we also choose to ignore ship collision calculations during navigation.
                # This will mean that you have a higher probability of crashing into ships, but it also means you will
                # make move decisions much quicker. As your skill progresses and your moves turn more optimal you may
                # wish to turn that option off.
                navigate_command = navigate(game_map,
                    ship,
                    closest_point_to(ship, planet),
                    speed = round(Int, Constants.MAX_SPEED/2),
                    ignore_ships = true)
                # If the move is possible, add it to the command_queue (if there are too many obstacles on the way
                # or we are trapped (or we reached our destination!), navigate_command will return empty string;
                # don't fret though, we can run the command again the next turn)
                if !isempty(navigate_command)
                    push!(command_queue, navigate_command)
                end
            end
            break
        end
    end

    # Send our set of commands to the Halite engine for this turn
    debug(logger, join(command_queue, ";"))
    send_command_queue(command_queue)
    turn += 1
    # TURN END
end
# GAME END

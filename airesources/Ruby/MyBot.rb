# Welcome to your first Halite-II bot!
#
# This bot's name is Opportunity. It's purpose is simple (don't expect it to win
# complex games :) ):
#  1. Initialize game
#  2. If a ship is not docked and there are unowned planets
#   a. Try to Dock in the planet if close enough
#   b. If not, go towards the planet

# Load the files we need
$:.unshift(File.dirname(__FILE__) + "/hlt")
require 'game'

# GAME START

# Here we define the bot's name as Opportunity and initialize the game, including
# communication with the Halite engine.
game = Game.new("Opportunity")
# We print our start message to the logs
game.logger.info("Starting my Opportunity bot!")

while true
  # TURN START
  # Update the map for the new turn and get the latest version
  game.update_map
  map = game.map

  # Here we define the set of commands to be sent to the Halite engine at the
  # end of the turn
  command_queue = []

  # For each ship we control
  map.me.ships.each do |ship|
    # if the ship is docked
    if ship.docking_status != Ship::DockingStatus::UNDOCKED
      # skip this ship
      next
    end

    # For each planet in the game (only non-destroyed planets are included)
    map.planets.each do |planet|
      # If the planet is owned
      if planet.owned?
        next
      end

      # If we can dock, let's (try to) dock. If two enemy ships try to dock at
      # once, neither will be able to.
      if ship.can_dock?(planet)
        command_queue << ship.dock(planet)
      else
        # If we can't dock, we move towards the closest empty point near this
        # planet (by using closest_point_to) with constant speed. Don't worry
        # about pathfinding for now, as the command will do it for you.
        # We run this navigate command each turn until we arrive to get the
        # latest move.
        # Here we move at half our maximum speed to better control the ships
        # In order to execute faster we also choose to ignore ship collision
        # calculations during navigation.
        # This will mean that you have a higher probability of crashing into
        # ships, but it also means you will make move decisions much quicker.
        # As your skill progresses and your moves turn more optimal you may
        # wish to turn that option off.
        closest = ship.closest_point_to(planet)
        speed = Game::Constants::MAX_SPEED/2
        navigate_command = ship.navigate(closest, map, speed, ignore_ships: true)
        # If the move is possible, add it to the command_queue (if there are too
        # many obstacles on the way or we are trapped (or we reached our
        # destination!), navigate_command will return null; don't fret though,
        # we can run the command again the next turn)
        if navigate_command
          command_queue << navigate_command
        end
      end
      break
    end
  end
  game.send_command_queue(command_queue)
end

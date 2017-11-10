logger = get_logger("bot-logger")
set_level(logger, "debug")

struct Game
    id::String
    botname::String
    width::Int
    height::Int
    initial_game_map::GameMap
end
function Game(botname::String)
    id = readline()
    botname = "$botname-$id"
    width, height = parse.(Int, split(readline()))
    initial_game_map = GameMap(id, width, height, readline())
    Game(id, botname, width, height, initial_game_map)
end

GameMap(game::Game, map_string::String) = GameMap(game.id, game.width, game.height, map_string)

"""
update_map(game::Game)

Parse the map given by the engine.
"""
update_map(game::Game) = GameMap(game, readline())

"""
    start_game(game)

This function should be used after bot initialization and all preliminary checks and warmup.
"""
start_game(game::Game) = print(game.botname, "\n")

"""
    setup_logger(name, id)

Set up and truncate the log. Log name is "{id}-{name}.log".
"""
function setup_logger(name, id)
    add_handler(logger, DefaultHandler(open("$id-$name.log", "w"), 
                    DefaultFormatter("[{date} | {level} | {name}]: {msg}"))) 
    remove_handler(logger, "console")
end

"""
    send_command_queue(command_queue::Vector{String})

Issue the given list of commands.
"""
function send_command_queue(command_queue::Vector{String}) 
    print.(command_queue)
    print("\n")
end
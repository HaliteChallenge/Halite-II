using Memento

logger = get_logger("bot-logger")
set_level(logger, "debug")

struct Game
    id::String
    botname::String
    initial_game_map::GameMap
    width::Int
    height::Int

    function Game(botname::String)
        id = get_msg()
        setup_logger(botname, id)
        botname = @sprintf("%s-%s", botname, id)

        width, height = map(x -> parse(Int, x), split(get_msg()))

        initial_game_map = GameMap(id, width, height, get_msg())
        debug(logger, @sprintf("Width: %d, Height: %d", width, height))
        for planet in all_planets(initial_game_map)
            debug(logger, @sprintf("Planet: %s, x: %d, y: %d", planet.id, planet.x, planet.y))
        end

        new(id, botname, initial_game_map, width, height)
    end
end
GameMap(game::Game, map_string::String) = GameMap(game.id, game.width, game.height, map_string)

"""
    start_game(game)

This function should be used after bot initialization and all preliminary checks and warmsup.
"""
function start_game(game::Game)
    send_msg(game.botname, true)
end

"""
    update_map(game::Game)

Parse the map given by the engine.
"""
function update_map(game::Game)
    GameMap(game, get_msg())
end

"""
    setup_logger(name, id)

Set up and truncate the log. Log name is "{name}-{id}.log".
"""
function setup_logger(name, id)
    add_handler(logger, DefaultHandler(open(@sprintf("%s-%s.log", name, id), "w"), DefaultFormatter("[{date} | {level} | {name}]: {msg}"))) 
    remove_handler(logger, "console")
end

"""
    get_msg()

Read input from the game.
"""
get_msg() = readline(STDIN)

"""
    send_msg(s, done=false)

Send data to the game. Call `done_msg` once finished or specify `done`=true.
"""
function send_msg(s::String, done::Bool=false)
    if done
        write(STDOUT, s*"\n")
    else
        write(STDOUT, s)
    end
    flush(STDOUT)
end

"""
    done_msg()

Finish sending commands to the game.
"""
done_msg() = send_msg("\n")

"""
    send_command_queue(command_queue::Vector{String})

Issue the given list of commands.
"""
function send_command_queue(command_queue::Vector{String})
    for command in command_queue
        send_msg(command)
    end
    done_msg()
end

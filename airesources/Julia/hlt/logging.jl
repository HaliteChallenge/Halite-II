using Memento

function set_logger(game::Game)
    logger = getlogger(current_module())            
    logger.handlers = Dict{Any,Memento.Handler}()   # Remove console handler
    push!(logger, DefaultHandler(string(game.botname, "_", game.id, ".log")))

    setlevel!(logger, "info")

    for i in values(logger.handlers)
        i.fmt = DefaultFormatter("{msg}")   # Rewrite logging message format
    end

    return logger
end

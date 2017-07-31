import logging
import random

import sqlalchemy

from .. import config, model, response_success, util


def rand_map_size():
    # Pick map size. Duplicate entries are used to weight the
    # probability of a particular size
    map_sizes = [96, 96, 128, 128, 128, 160, 160, 160, 160,
                 192, 192, 192, 256]
    map_width = random.choice(map_sizes)
    map_height = random.choice(map_sizes)

    # Width, height
    return max(map_width, map_height), min(map_width, map_height)


def serve_game_task(conn, has_gpu=False):
    """Try to find a set of players to play a game together."""
    # Only allow 2 or 4 player games
    player_count = 2 if random.random() > 0.5 else 4

    # If there is a GPU, only take bots from players who qualify for the GPU.
    # Else, do not run games for players who qualify for one.
    total_players = conn.execute(model.total_ranked_users).first()[0]
    thresholds = util.tier_thresholds(total_players)
    ranked_users = model.ranked_users_query()
    if has_gpu or config.COMPETITION_FINALS_PAIRING:
        rank_limit = (ranked_users.c.rank <=
                      thresholds[config.GPU_TIER_NAME])
    else:
        rank_limit = (ranked_users.c.rank >
                      thresholds[config.GPU_TIER_NAME])

    seed_player = find_seed_player(conn, ranked_users, rank_limit)
    if not seed_player:
        return

    print(seed_player)
    # Select the rest of the players
    mu_rank_limit = int(5.0 / (0.01 + random.random()) ** 0.65)

    logging.info(
        "Matchmaking: seed player: ID {}, mu {}, "
        "maximum rank distance {}".format(
            seed_player.user_id, seed_player.mu, mu_rank_limit))

    # Find closely matched players
    sqlfunc = sqlalchemy.sql.func
    close_players = sqlalchemy.sql.select([
        model.ranked_bots_users.c.user_id,
        model.ranked_bots_users.c.bot_id,
        ranked_users.c.username,
        model.ranked_bots_users.c.rank,
        ranked_users.c.rank.label("player_rank"),
        model.ranked_bots_users.c.num_submissions.label("version_number"),
        model.ranked_bots_users.c.mu,
    ]).select_from(
        model.ranked_bots_users.join(
            model.bots,
            (model.ranked_bots_users.c.user_id == model.bots.c.user_id) &
            (model.ranked_bots_users.c.bot_id == model.bots.c.id)
        ).join(
            ranked_users,
            (ranked_users.c.user_id == model.ranked_bots_users.c.user_id)
        )
    ).where(
        (model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value) &
        (~((model.bots.c.user_id == seed_player["user_id"]) &
           (model.bots.c.id == seed_player["bot_id"]))) &
        rank_limit
    ).order_by(
        sqlalchemy.func.abs(model.bots.c.mu - seed_player["mu"])
    ).limit(mu_rank_limit).alias("muranktable")

    # Select more bots than needed, discard ones from same player
    query = close_players.select().order_by(sqlfunc.rand()) \
        .limit(player_count * 2)
    potential_players = conn.execute(query).fetchall()
    players = [seed_player]
    player_ids = {seed_player["user_id"]}
    for player in potential_players:
        if player["user_id"] in player_ids:
            continue
        else:
            players.append(player)
            player_ids.add(player["user_id"])

        if len(players) == player_count:
            break

    map_width, map_height = rand_map_size()
    players = [{
        "user_id": player["user_id"],
        "bot_id": player["bot_id"],
        "username": player["username"],
        "version_number": player["version_number"],
        "rank": player["rank"],
        "tier": util.tier(player["rank"], total_players),
        "player_rank": player["player_rank"],
        "player_tier": util.tier(player["player_rank"], total_players),
    } for player in players]

    if len(players) == player_count:
        return response_success({
            "type": "game",
            "width": map_width,
            "height": map_height,
            "users": players,
        })


def rand_map_size():
    # Pick map size. Duplicate entries are used to weight the
    # probability of a particular size
    map_sizes = [96, 96, 128, 128, 128, 160, 160, 160, 160,
                 192, 192, 192, 256]
    map_width = random.choice(map_sizes)
    map_height = random.choice(map_sizes)

    # Width, height
    return max(map_width, map_height), min(map_width, map_height)


def serve_game_task(conn, has_gpu=False):
    """Try to find a set of players to play a game together."""
    # Only allow 2 or 4 player games
    player_count = 2 if random.random() > 0.5 else 4

    # If there is a GPU, only take bots from players who qualify for the GPU.
    # Else, do not run games for players who qualify for one.
    total_players = conn.execute(model.total_ranked_users).first()[0]
    thresholds = util.tier_thresholds(total_players)
    ranked_users = model.ranked_users_query()
    if has_gpu or config.COMPETITION_FINALS_PAIRING:
        rank_limit = (ranked_users.c.rank <=
                      thresholds[config.GPU_TIER_NAME])
    else:
        rank_limit = (ranked_users.c.rank >
                      thresholds[config.GPU_TIER_NAME])

    seed_player = find_seed_player(conn, ranked_users, rank_limit)
    if not seed_player:
        return

    print(seed_player)
    # Select the rest of the players
    mu_rank_limit = int(5.0 / (0.01 + random.random()) ** 0.65)

    logging.info(
        "Matchmaking: seed player: ID {}, mu {}, "
        "maximum rank distance {}".format(
            seed_player.user_id, seed_player.mu, mu_rank_limit))

    # Find closely matched players
    sqlfunc = sqlalchemy.sql.func
    close_players = sqlalchemy.sql.select([
        model.ranked_bots_users.c.user_id,
        model.ranked_bots_users.c.bot_id,
        ranked_users.c.username,
        model.ranked_bots_users.c.rank,
        ranked_users.c.rank.label("player_rank"),
        model.ranked_bots_users.c.num_submissions.label("version_number"),
        model.ranked_bots_users.c.mu,
    ]).select_from(
        model.ranked_bots_users.join(
            model.bots,
            (model.ranked_bots_users.c.user_id == model.bots.c.user_id) &
            (model.ranked_bots_users.c.bot_id == model.bots.c.id)
        ).join(
            ranked_users,
            (ranked_users.c.user_id == model.ranked_bots_users.c.user_id)
        )
    ).where(
        (model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value) &
        (~((model.bots.c.user_id == seed_player["user_id"]) &
           (model.bots.c.id == seed_player["bot_id"]))) &
        rank_limit
    ).order_by(
        sqlalchemy.func.abs(model.bots.c.mu - seed_player["mu"])
    ).limit(mu_rank_limit).alias("muranktable")

    # Select more bots than needed, discard ones from same player
    query = close_players.select().order_by(sqlfunc.rand()) \
        .limit(player_count * 2)
    potential_players = conn.execute(query).fetchall()
    players = [seed_player]
    player_ids = {seed_player["user_id"]}
    for player in potential_players:
        if player["user_id"] in player_ids:
            continue
        else:
            players.append(player)
            player_ids.add(player["user_id"])

        if len(players) == player_count:
            break

    map_width, map_height = rand_map_size()
    players = [{
        "user_id": player["user_id"],
        "bot_id": player["bot_id"],
        "username": player["username"],
        "version_number": player["version_number"],
        "rank": player["rank"],
        "tier": util.tier(player["rank"], total_players),
        "player_rank": player["player_rank"],
        "player_tier": util.tier(player["player_rank"], total_players),
    } for player in players]

    if len(players) == player_count:
        return response_success({
            "type": "game",
            "width": map_width,
            "height": map_height,
            "users": players,
        })




def find_recent_seed_player(conn, ranked_users, rank_limit, restrictions=False):
    """
    Find a seed player that has played in a recent game.
    :param conn:
    :param rank_limit:
    :param restrictions: If True, additionally restrict number of games
    played by the bot, and sort randomly.
    :return:
    """
    # Find the users in the most recent games, and pick a seed player
    # from those. Ported from Halite 1 PHP backend (which seems to have
    # been derived from aichallenge/Ants).

    sqlfunc = sqlalchemy.sql.func

    max_time = sqlfunc.max(model.games.c.time_played).label("max_time")
    user_id = model.game_participants.c.user_id
    bot_id = model.game_participants.c.bot_id

    recent_gamers = sqlalchemy.sql.select([
        max_time,
        user_id,
        bot_id,
        ranked_users.c.username,
        ranked_users.c.rank.label("player_rank"),
        model.ranked_bots_users.c.num_submissions.label("version_number"),
        model.ranked_bots_users.c.mu,
        model.ranked_bots_users.c.rank,
    ]).select_from(
        model.game_participants.join(
            model.games,
            model.games.c.id == model.game_participants.c.game_id,
            ).join(
            model.ranked_bots_users,
            (model.ranked_bots_users.c.user_id ==
             model.game_participants.c.user_id) &
            (model.ranked_bots_users.c.bot_id == model.game_participants.c.bot_id)
        ).join(
            ranked_users,
            (model.ranked_bots_users.c.user_id == ranked_users.c.user_id) &
            rank_limit
        )
    ).group_by(user_id, bot_id, model.ranked_bots_users.c.rank).reduce_columns().alias("temptable")

    # Of those users, select ones with under 400 games, preferring
    # ones in older games, and get their data
    outer_bots = model.bots.alias("bot")
    bot_restrictions = (outer_bots.c.compile_status ==
                        model.CompileStatus.SUCCESSFUL.value)
    if restrictions:
        bot_restrictions &= outer_bots.c.games_played < 400

    potential_players = sqlalchemy.sql.select([
        recent_gamers.c.user_id,
        recent_gamers.c.bot_id,
        recent_gamers.c.username,
        recent_gamers.c.version_number,
        recent_gamers.c.mu,
        recent_gamers.c.rank,
        recent_gamers.c.player_rank,
    ]).select_from(
        recent_gamers.join(
            outer_bots,
            (outer_bots.c.user_id == recent_gamers.c.user_id) &
            (outer_bots.c.id == recent_gamers.c.bot_id))) \
        .where(bot_restrictions) \
        .order_by(recent_gamers.c.max_time.asc()) \
        .limit(15).alias("orderedtable")

    if restrictions:
        # Then sort them randomly and take one
        query = potential_players.select().order_by(sqlfunc.rand()).limit(1)
    else:
        query = potential_players.select().limit(1)

    return conn.execute(query).first()


def find_newbie_seed_player(conn, ranked_users, rank_limit):
    """
    Find a seed player that has not played that many games.
    :param conn:
    :param rank_limit:
    :return:
    """
    sqlfunc = sqlalchemy.sql.func
    ordering = sqlfunc.rand() * -sqlfunc.pow(model.bots.c.sigma, 2)
    query = sqlalchemy.sql.select([
        ranked_users.c.user_id,
        model.bots.c.id.label("bot_id"),
        ranked_users.c.username,
        model.ranked_bots_users.c.rank,
        ranked_users.c.rank.label("player_rank"),
        model.bots.c.version_number,
        model.bots.c.mu,
    ]).select_from(
        model.ranked_bots_users.join(
            model.bots,
            (model.ranked_bots_users.c.user_id == model.bots.c.user_id) &
            (model.ranked_bots_users.c.bot_id == model.bots.c.id)
        ).join(
            ranked_users,
            model.ranked_bots_users.c.user_id == ranked_users.c.user_id
        )
    ).where(
        (model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value) &
        (model.bots.c.games_played < 400) &
        rank_limit
    ).order_by(ordering).limit(1).reduce_columns()

    return conn.execute(query).first()


def find_seed_player(conn, ranked_users, rank_limit):
    """
    Find a seed player for a game.
    :param conn: A database connection.
    :param rank_users: The table clause to base player ranks on.
    :param rank_limit: A WHERE clause to restrict player ranks.
    :return: A seed player, or None.
    """
    if not config.COMPETITION_FINALS_PAIRING:
        rand_value = random.random()

        if rand_value > 0.5:
            logging.info("Matchmaking: seed player: looking for random bot"
                         " with under 400 games played")
            result = find_newbie_seed_player(conn, ranked_users, rank_limit)
            if result:
                return result
        elif 0.25 < rand_value <= 0.5:
            logging.info("Matchmaking: seed player: looking for random bot"
                         " from recent game with under 400 games played")
            result = find_recent_seed_player(conn, ranked_users, rank_limit,
                                             restrictions=True)
            if result:
                return result

        # Fallback case
        # Same as the previous case, but we do not restrict how many
        # games the user can have played, and we do not sort randomly.
        logging.info("Matchmaking: seed player: looking for random bot"
                     " from recent game")
        result = find_recent_seed_player(conn, ranked_users, rank_limit,
                                         restrictions=False)
        if result:
            return result
    else:
        # For finals: take 15 players with least games played, and select one
        total_players = conn.execute(model.total_ranked_users).first()[0]
        thresholds = util.tier_thresholds(total_players)
        rank_limit = (model.ranked_bots_users.c.rank <=
                      thresholds[config.FINALS_TIER_NAME])

        least_played = sqlalchemy.sql.select([
            model.ranked_bots_users.c.user_id,
            model.ranked_bots_users.c.bot_id,
            model.ranked_bots_users.c.username,
            model.ranked_bots_users.c.rank,
            ranked_users.c.rank.label("player_rank"),
            model.ranked_bots_users.c.mu,
            model.ranked_bots_users.c.num_submissions.label("version_number"),
        ]).select_from(
            model.ranked_bots_users.join(
                ranked_users,
                (model.ranked_bots_users.c.user_id == ranked_users.c.user_id) &
                rank_limit
            )
        ).where(
            model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value
        ).order_by(model.bots.c.games_played.asc()).limit(15).alias("least_played")

        query = least_played.select().order_by(
            sqlalchemy.sql.func.rand()
        ).limit(1)

        return conn.execute(query).first()

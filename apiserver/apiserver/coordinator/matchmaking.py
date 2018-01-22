import collections
import datetime
import logging
import random

import sqlalchemy

from .. import config, model, util


def rand_map_size():
    # Pick map size. Duplicate entries are used to weight the
    # probability of a particular size
    map_sizes = [80, 80, 88, 88, 96, 96, 96, 104, 104, 104, 104,
                 112, 112, 112, 120, 120, 128, 128]
    base_size = random.choice(map_sizes)
    # Always generate 3:2 aspect ratio
    map_width = 3 * base_size
    map_height = 2 * base_size

    # Width, height
    return max(map_width, map_height), min(map_width, map_height)


def serve_game_task(conn, has_gpu=False):
    """Try to find a set of players to play a game together."""
    if not config.COMPETITION_FINALS_PAIRING and random.random() < 0.1:
        result = find_challenge(conn, has_gpu)
        if result:
            return result

    # Only allow 2 or 4 player games
    player_count = 2 if random.random() > 0.5 else 4

    # If there is a GPU, only take bots from players who qualify for the GPU.
    # Else, do not run games for players who qualify for one.
    total_players = conn.execute(model.total_ranked_users).first()[0]
    thresholds = util.tier_thresholds(total_players)
    sqlfunc = sqlalchemy.sql.func
    ranked_users = model.ranked_users_query()
    if config.COMPETITION_FINALS_PAIRING:
        last_game_id = conn.execute(sqlalchemy.sql.select([
                sqlfunc.max(model.games.c.id)
            ]).select_from(model.games)).first()[0]
        try:
            cur_final_games = last_game_id - config.LAST_OPEN_GAME
        except TypeError:
            raise Exception("Final open game id must be set in config.py.")
        finals_rank_limit = total_players + 1
        for start, cutoff in config.FINALS_CUTOFF_SCHEDULE:
            if start > cur_final_games:
                break
            finals_rank_limit = cutoff
        rank_limit = (ranked_users.c.rank <= finals_rank_limit)
    else:
        rank_limit = sqlalchemy.sql.expression.true()

    seed_filter = rank_limit
    if config.ENFORCE_GPU_SEEDING:
        if has_gpu:
            seed_filter = (rank_limit
                          & (model.ranked_bots_users.c.is_gpu_enabled == True))
        else:
            seed_filter = (rank_limit
                          & (model.ranked_bots_users.c.is_gpu_enabled == False))
    elif not has_gpu:
        # Even if we don't require that GPU workers only receive games
        # seeded with GPU bots, we still don't want GPU bots to get
        # seeded on a non-GPU worker.
        seed_filter = (rank_limit
                       & (model.ranked_bots_users.c.is_gpu_enabled == False))

    seed_player = find_seed_player(conn, ranked_users, seed_filter)
    if not seed_player and has_gpu:
        # If there isn't a gpu enabled seed, check for any seed to keep the
        # gpu worker busy
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
    player_filter = sqlalchemy.sql.expression.true()
    if not has_gpu:
        # if this isn't a gpu enabled worker filter out gpu only bots
        player_filter = (model.ranked_bots_users.c.is_gpu_enabled == False)
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
        player_filter
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
        return util.response_success({
            "type": "game",
            "width": map_width,
            "height": map_height,
            "users": players,
            "challenge": None,
        })


def reset_challenges(conn):
    """Check ongoing challenges, and reset ones that are "stuck"."""
    reset_stuck_challenges = model.challenges.update().where(
        (model.challenges.c.status == model.ChallengeStatus.PLAYING_GAME.value) &
        (model.challenges.c.most_recent_game_task <
         datetime.datetime.now() - datetime.timedelta(
             minutes=30))
    ).values(
        status=model.ChallengeStatus.CREATED.value,
    )
    conn.execute(reset_stuck_challenges)


def find_challenge(conn, has_gpu=False):
    """
    Find a set of players in a challenge.
    """
    # Reset any stuck challenges
    reset_challenges(conn)

    challenge = conn.execute(
        model.challenges.select(
            (model.challenges.c.status == model.ChallengeStatus.CREATED.value) |
            # Also pick up any stuck challenges
            (
                (model.challenges.c.status == model.ChallengeStatus.PLAYING_GAME.value) &
                (model.challenges.c.most_recent_game_task <
                 datetime.datetime.now() - datetime.timedelta(minutes=30))
            )
        ).order_by(
            model.challenges.c.most_recent_game_task.asc()
        )
    ).first()

    if not challenge:
        return None

    player_filter = sqlalchemy.sql.expression.true()
    if not has_gpu:
        player_filter = (model.ranked_bots_users.c.is_gpu_enabled == False)

    total_players = conn.execute(model.total_ranked_users).first()[0]
    thresholds = util.tier_thresholds(total_players)
    ranked_users = model.ranked_users_query()
    bots_query = sqlalchemy.sql.select([
        model.ranked_bots_users.c.user_id,
        model.ranked_bots_users.c.bot_id,
        model.ranked_bots_users.c.username,
        model.ranked_bots_users.c.rank,
        model.ranked_bots_users.c.num_submissions.label("version_number"),
    ]).select_from(
        model.ranked_bots_users.join(
            model.bots,
            (model.ranked_bots_users.c.user_id == model.bots.c.user_id) &
            (model.ranked_bots_users.c.bot_id == model.bots.c.id)
        )
    ).where(
        (model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value) &
        sqlalchemy.sql.exists(
            model.challenge_participants.select(
                (model.ranked_bots.c.user_id == model.challenge_participants.c.user_id) &
                (model.challenge_participants.c.challenge_id == challenge["id"]) &
                (model.challenge_participants.c.user_id == model.ranked_bots_users.c.user_id)
            )
        ) &
        player_filter
    )
    bots = conn.execute(bots_query).fetchall()

    user_bots = collections.defaultdict(list)
    for bot in bots:
        user_bots[bot["user_id"]].append(bot)

    if len(user_bots) < 2 or challenge["issuer"] not in user_bots:
        # We had to skip this challenge, but give it a time anyways,
        # so that we don't get stuck on it
        conn.execute(model.challenges.update().values(
            most_recent_game_task=sqlalchemy.sql.func.now(),
        ).where(model.challenges.c.id == challenge["id"]))

        return None

    selected_bots = [random.choice(user_bots[challenge["issuer"]])]
    del user_bots[challenge["issuer"]]

    candidate_users = list(user_bots.keys())
    if len(user_bots) >= 3:
        # If 4-player challenge, always issue a 4-player game
        random.shuffle(candidate_users)
        selected_bots.extend([random.choice(user_bots[user_id])
                              for user_id in candidate_users[:3]])
    else:
        selected_bots.append(random.choice(user_bots[random.choice(candidate_users)]))

    map_width, map_height = rand_map_size()
    players = [{
        "user_id": player["user_id"],
        "bot_id": player["bot_id"],
        "username": player["username"],
        "version_number": player["version_number"],
        "rank": player["rank"],
        "tier": util.tier(player["rank"], total_players),
    } for player in selected_bots]

    conn.execute(model.challenges.update().values(
        status=model.ChallengeStatus.PLAYING_GAME.value,
        most_recent_game_task=sqlalchemy.sql.func.now(),
    ).where(model.challenges.c.id == challenge["id"]))

    return util.response_success({
        "type": "game",
        "width": map_width,
        "height": map_height,
        "users": players,
        "challenge": challenge["id"],
    })


def find_idle_seed_player(conn, ranked_users, seed_filter, restrictions=False):
    """
    Find a seed player that hasn't played recently.
    :param conn:
    :param seed_filter: SQL expression to limit which players can be used.
    :param restrictions: If True, additionally restrict number of games
    played by the bot, and sort randomly.
    :return:
    """
    # Get all users last time to play a game, and pick a seed player
    # from those that haven't played for the longest. Newly submitted bots
    # having never played get picked first. Initially ported from Halite 1
    # PHP backend (which was partially derived from aichallenge/Ants).

    sqlfunc = sqlalchemy.sql.func

    max_time = sqlfunc.max(model.games.c.time_played).label("max_time")
    user_id = model.game_participants.c.user_id
    bot_id = model.game_participants.c.bot_id

    gamers_last_play = sqlalchemy.sql.select([
        max_time,
        ranked_users.c.user_id,
        model.ranked_bots_users.c.bot_id,
        ranked_users.c.username,
        ranked_users.c.rank.label("player_rank"),
        model.ranked_bots_users.c.num_submissions.label("version_number"),
        model.ranked_bots_users.c.mu,
        # The database isn't smart enough to know that there is only one rank
        # value for a given (user_id, bot_id).
        sqlfunc.any_value(model.ranked_bots_users.c.rank).label('rank'),
    ]).select_from(
        ranked_users.join(
            model.ranked_bots_users,
            (model.ranked_bots_users.c.user_id
             == ranked_users.c.user_id)
            & seed_filter
        ).join(
            model.game_participants.join(
                model.games,
                model.games.c.id == model.game_participants.c.game_id
            ),
            (ranked_users.c.user_id == model.game_participants.c.user_id)
            & (model.ranked_bots_users.c.bot_id
               == model.game_participants.c.bot_id),
            isouter=True
        )
    ).group_by(ranked_users.c.user_id, model.ranked_bots_users.c.bot_id
    ).reduce_columns().alias("temptable")

    # Of those users, select ones with under 400 games, preferring
    # ones in older games, and get their data
    outer_bots = model.bots.alias("bot")
    bot_restrictions = (outer_bots.c.compile_status ==
                        model.CompileStatus.SUCCESSFUL.value)
    if restrictions:
        bot_restrictions &= outer_bots.c.games_played < 400

    potential_players = sqlalchemy.sql.select([
        gamers_last_play.c.user_id,
        gamers_last_play.c.bot_id,
        gamers_last_play.c.username,
        gamers_last_play.c.version_number,
        gamers_last_play.c.mu,
        gamers_last_play.c.rank,
        gamers_last_play.c.player_rank,
    ]).select_from(
        gamers_last_play.join(
            outer_bots,
            (outer_bots.c.user_id == gamers_last_play.c.user_id) &
            (outer_bots.c.id == gamers_last_play.c.bot_id))) \
        .where(bot_restrictions) \
        .order_by(gamers_last_play.c.max_time.asc()) \
        .limit(15).alias("orderedtable")

    if restrictions:
        # Then sort them randomly and take one
        query = potential_players.select().order_by(sqlfunc.rand()).limit(1)
    else:
        query = potential_players.select().limit(1)

    return conn.execute(query).first()


def find_newbie_seed_player(conn, ranked_users, seed_filter):
    """
    Find a seed player that has not played that many games.
    :param conn:
    :param seed_filter: SQL expression to limit which players can be used.
    :return:
    """
    sqlfunc = sqlalchemy.sql.func
    game_curve = 1 / (model.bots.c.games_played + 1)
    # weight the curve between half and full weight
    rand_factor = 0.5 + (sqlfunc.rand() * 0.5)
    # Since the curve is cut in half every doubling of games, this means a bot
    # will always be chosen ahead of any bots that have more than twice the
    # number of games
    ordering = rand_factor * -game_curve
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
        seed_filter
    ).order_by(ordering).limit(20).reduce_columns().alias("seed_query").select(
    ).order_by(sqlfunc.rand())

    return conn.execute(query).first()


def find_seed_player(conn, ranked_users, seed_filter):
    """
    Find a seed player for a game.
    :param conn: A database connection.
    :param rank_users: The table clause to base player ranks on.
    :param seed_filter: A WHERE clause to restrict player ranks.
    :return: A seed player, or None.
    """
    if not config.COMPETITION_FINALS_PAIRING:
        rand_value = random.random()

        # Give 25% of games to least recently played new players
        if rand_value > 0.75:
            logging.info("Matchmaking: seed player: looking for random bot"
                         " with under 400 games played that hasn't played"
                         " recently")
            result = find_idle_seed_player(conn, ranked_users, seed_filter,
                                             restrictions=True)
            if result:
                return result
        # Give 65% of games to lowest games played
        # (also take any games where a seed isn't found above, almost certainly
        #  because everyone is over 400 games)
        if rand_value > 0.1:
            logging.info("Matchmaking: seed player: looking for random bot"
                         " with fewest games played")
            result = find_newbie_seed_player(conn, ranked_users, seed_filter)
            if result:
                return result

        # Give 10% of games to overall user who has least recently played
        logging.info("Matchmaking: seed player: looking for random bot"
                     " that hasn't played recently")
        result = find_idle_seed_player(conn, ranked_users, seed_filter,
                                         restrictions=False)
        if result:
            return result
    else:
        # For finals take players with least games played
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
                # This join should have no effect on the result but it makes
                # the query 30-40 times faster.
                model.bots,
                (model.ranked_bots_users.c.user_id == model.bots.c.user_id) &
                (model.ranked_bots_users.c.bot_id == model.bots.c.id)
            ).join(
                ranked_users,
                (model.ranked_bots_users.c.user_id == ranked_users.c.user_id) &
                seed_filter
            )
        ).where(
            model.bots.c.compile_status == model.CompileStatus.SUCCESSFUL.value
        ).order_by(model.bots.c.games_played.asc()
        ).limit(20).alias("least_played").select().order_by(
            sqlalchemy.sql.func.rand()
        ).limit(1)

        return conn.execute(least_played).first()

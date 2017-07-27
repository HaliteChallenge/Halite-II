import base64
import binascii
import datetime
import io
import json
import logging
import os
import random

import flask
import sqlalchemy
import trueskill

import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions

from .. import config, model, notify, response_success, util


coordinator_api = flask.Blueprint("coordinator_api", __name__)


def reset_compilation_tasks(conn):
    """Check ongoing compilation tasks, and reset ones that are "stuck"."""
    reset_stuck_tasks = model.bots.update().where(
        (model.bots.c.compile_status == "InProgress") &
        (model.bots.c.compile_start <
         datetime.datetime.now() - datetime.timedelta(
             minutes=config.COMPILATION_STUCK_THRESHOLD))
    ).values(
        compile_status="Uploaded",
        compile_start=None,
    )
    conn.execute(reset_stuck_tasks)


def serve_compilation_task(conn):
    """Try to find and return a compilation task."""
    with conn.begin() as transaction:
        # Try to assign a compilation task.
        find_compilation_task = model.bots.select() \
            .where(model.bots.c.compile_status == "Uploaded") \
            .order_by(model.bots.c.user_id.asc()) \
            .limit(1)
        bot = conn.execute(find_compilation_task).first()
        if bot:
            user_id = bot["user_id"]
            bot_id = bot["id"]
            # Keep track of when compilation started, so that we can
            # restart it if it gets stuck
            update = model.bots.update() \
                .where((model.bots.c.user_id == user_id) &
                       (model.bots.c.id == bot_id)) \
                .values(compile_status="InProgress",
                        compile_start=sqlalchemy.sql.func.now())
            conn.execute(update)
            return response_success({
                "type": "compile",
                "user": user_id,
                "bot": bot_id,
            })
    return None


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
    total_players = conn.execute(model.total_ranked_bots).first()[0]
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
        (model.bots.c.compile_status == "Successful") &
        (~((model.bots.c.user_id == seed_player["user_id"]) &
           (model.bots.c.id == seed_player["bot_id"]))) &
        rank_limit
    ).order_by(
        sqlalchemy.func.abs(model.bots.c.mu - seed_player["mu"])
    ).limit(mu_rank_limit).alias("muranktable")

    # Select more bots than needed, discard ones from same player
    query = close_players.select().order_by(sqlfunc.rand())\
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


@coordinator_api.route("/task")
def task():
    """Serve compilation and game tasks to worker instances."""

    capabilities = flask.request.args.getlist("capability")
    has_gpu = "gpu" in capabilities

    with model.engine.connect() as conn:
        # Prioritize compiling new bots; don't use a GPU instance on this
        # task, though
        if not has_gpu:
            reset_compilation_tasks(conn)
            # response = serve_compilation_task(conn)
            # if response:
            #     return response

        # Otherwise, play a game
        # If the worker has a GPU, try really hard to give it some work to do
        tries = 0
        while tries == 0 or ((has_gpu or config.COMPETITION_FINALS_PAIRING)
                             and tries < 10):
            response = serve_game_task(conn, has_gpu=has_gpu)
            if response:
                return response
            tries += 1

    return response_success({
        "type": "notask",
    })


# TODO: these aren't RESTful URLs, but it's what the worker expects
@coordinator_api.route("/compile", methods=["POST"])
def update_compilation_status():
    """Update the compilation status of a bot."""
    user_id = flask.request.form.get("user_id", None)
    bot_id = flask.request.form.get("bot_id", None)
    did_compile = bool(int(flask.request.form.get("did_compile", '0')))
    language = flask.request.form.get("language", "Other")
    errors = flask.request.form.get("errors", "")

    if user_id is None:
        raise util.APIError(400, message="Must provide user ID.")

    with model.engine.connect() as conn:
        user = conn.execute(model.users.select(
            model.users.c.id == user_id
        )).first()
        bot = conn.execute(model.bots.select(
            (model.bots.c.user_id == user_id) &
            (model.bots.c.id == bot_id)
        )).first()

        bot_rank = conn.execute(sqlalchemy.sql.select([
            sqlalchemy.sql.text("ranked_bots.bot_rank")
        ]).where(
            (model.ranked_bots.c.user_id == user_id) &
            (model.ranked_bots.c.bot_id == bot_id)
        )).first()

        if not user:
            raise util.APIError(400, message="User not found.")
        if not bot:
            raise util.APIError(400, message="Bot not found.")

        update = model.bots.update() \
            .where((model.bots.c.user_id == user_id) &
                   (model.bots.c.id == bot_id)) \
            .values(
            compile_status="Successful" if did_compile else "Failed",
            compile_start=None,
        )
        conn.execute(update)

        if did_compile:
            # This is backwards of the order in the original PHP, but the
            # original PHP updated the table using the -old- values of the
            # User row. This ordering makes it clearer that this is
            # intentional.

            # If version number is 0, then this is the first time the bot
            # has ever been submitted, so there's nothing to put in the
            # history table.
            if bot["version_number"] != 0:
                num_active_users = conn.execute(
                    sqlalchemy.sql.select([
                        sqlalchemy.sql.func.count()
                    ]).select_from(model.users)
                ).first()[0]

                conn.execute(
                    model.bot_history.insert().values(
                        user_id=user_id,
                        bot_id=bot_id,
                        version_number=bot["version_number"],
                        # User is unranked if this is their first bot
                        last_rank=bot_rank[0] if bot_rank else None,
                        last_score=bot["score"],
                        last_num_players=num_active_users,
                        last_games_played=bot["games_played"],
                        language=bot["language"],
                    )
                )

            # We do not reset the bot rank for new submissions now.
            conn.execute(
                model.bots.update().where(
                    (model.bots.c.user_id == user_id) &
                    (model.bots.c.id == bot_id)
                ).values(
                    language=language,
                    version_number=model.bots.c.version_number + 1,
                    games_played=0,
                )
            )

            notify.send_notification(
                user["email"],
                user["username"],
                "Bot successfully compiled",
                notify.COMPILATION_SUCCESS)

            return response_success()
        else:
            notify.send_notification(
                user["email"],
                user["username"],
                "Bot failed to compile",
                notify.COMPILATION_FAILURE.format(language=language,
                                                  errors=errors))
            return response_success()


@coordinator_api.route("/botFile", methods=["POST"])
def upload_bot():
    """Save a compiled bot to object storage."""
    user_id = flask.request.form.get("user_id", None)
    bot_id = flask.request.form.get("bot_id", None)

    if "bot.zip" not in flask.request.files:
        raise util.APIError(400, message="Please provide the bot file.")

    uploaded_file = flask.request.files["bot.zip"]
    # Save to GCloud
    blob = gcloud_storage.Blob("{}_{}".format(user_id, bot_id),
                               model.get_bot_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)
    return response_success()


@coordinator_api.route("/botFile", methods=["GET"])
def download_bot():
    """Retrieve a compiled or uncompiled bot from object storage."""
    user_id = flask.request.values.get("user_id", None)
    bot_id = flask.request.values.get("bot_id", None)
    compile = flask.request.values.get("compile", False)

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    # Retrieve from GCloud
    try:
        botname = "{}_{}".format(user_id, bot_id)
        blob = gcloud_storage.Blob(botname,
                                   bucket, chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(buffer, mimetype="application/zip",
                               as_attachment=True,
                               attachment_filename=botname + ".zip")
    except gcloud_exceptions.NotFound:
        raise util.APIError(404, message="Bot not found.")


@coordinator_api.route("/botHash")
def hash_bot():
    """Get the MD5 hash of a compiled bot."""
    user_id = flask.request.args.get("user_id", None)
    bot_id = flask.request.args.get("bot_id", None)
    compile = flask.request.args.get("compile", False)

    if not user_id or not bot_id:
        raise util.APIError(400, message="Please provide user and bot ID.")

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    blob = bucket.get_blob("{}_{}".format(user_id, bot_id))
    if blob is None:
        raise util.APIError(400, message="Bot does not exist.")

    return response_success({
        "hash": binascii.hexlify(base64.b64decode(blob.md5_hash)).decode('utf-8'),
    })


@coordinator_api.route("/game", methods=["POST"])
def upload_game():
    """Save the results of a game into the database and object storage."""
    if ("game_output" not in flask.request.values or
            "users" not in flask.request.values):
        raise util.APIError(
            400, message="Please provide both the game output and users.")

    game_output = json.loads(flask.request.values["game_output"])
    users = json.loads(flask.request.values["users"])

    with model.engine.connect() as conn:
        total_users = conn.execute(model.total_ranked_bots).first()[0]
        for user in users:
            stored_user = conn.execute(
                sqlalchemy.sql.select([
                    model.users.c.id.label("user_id"),
                    model.users.c.on_email_list,
                    model.users.c.email,
                ]).where(model.users.c.id == user["user_id"])
            ).first()

            stored_bot = conn.execute(
                sqlalchemy.sql.select([
                    model.bots.c.version_number,
                    model.bots.c.mu,
                    model.bots.c.sigma,
                ]).where(
                    (model.bots.c.id == user["bot_id"]) &
                    (model.bots.c.user_id == user["user_id"])
                )
            ).first()

            stored_rank = conn.execute(
                sqlalchemy.sql.select([
                    model.ranked_bots_users.c.rank,
                ]).where(
                    (model.ranked_bots_users.c.bot_id == user["bot_id"]) &
                    (model.ranked_bots_users.c.user_id == user["user_id"])
                )
            ).first()

            if not stored_user or not stored_bot:
                raise util.APIError(400, message="User or bot doesn't exist")

            # If the user has submitted a new bot in the meanwhile,
            # ignore the game
            if stored_bot["version_number"] != user["version_number"]:
                return response_success({
                    "message": "User {} has uploaded a new bot, discarding "
                               "match.".format(user["user_id"])
                })

            user.update(dict(stored_user))
            user.update(dict(stored_bot))
            if stored_rank:
                user["rank"] = stored_rank["rank"]
                user["tier"] = util.tier(user["rank"], total_users)
            else:
                user["rank"] = None
                user["tier"] = util.tier(total_users, total_users)

    # Store the replay and any error logs
    replay_name = os.path.basename(game_output["replay"])
    replay_key, _ = os.path.splitext(replay_name)
    if replay_name not in flask.request.files:
        raise util.APIError(
            400, message="Replay file not found in uploaded files.")

    # Store replay in separate bucket if user is Gold/Plat/Diamond
    bucket_class = 0
    for user in users:
        if user["tier"] in (config.TIER_0_NAME, config.TIER_1_NAME,
                            config.TIER_2_NAME):
            bucket_class = 1
            break

    bucket = model.get_replay_bucket(bucket_class)
    blob = gcloud_storage.Blob(replay_key, bucket, chunk_size=262144)
    blob.upload_from_file(flask.request.files[replay_name])

    for user in users:
        if user["timed_out"]:
            error_log_name = user["log_name"]
            if error_log_name not in flask.request.files:
                raise util.APIError(
                    400,
                    message="Error log {} not found in uploaded files."
                            .format(error_log_name))

            error_log_key = user["log_name"] = \
                replay_key + "_error_log_" + str(user["user_id"])
            blob = gcloud_storage.Blob(error_log_key,
                                       model.get_error_log_bucket(),
                                       chunk_size=262144)
            blob.upload_from_file(flask.request.files[error_log_name])

    # TODO: the original code deletes games if there are over 600k in the
    # database. Is that really a concern for us?

    # Store game results in database
    with model.engine.connect() as conn:
        game_id = conn.execute(model.games.insert().values(
            replay_name=replay_key,
            map_width=game_output["map_width"],
            map_height=game_output["map_height"],
            map_seed=game_output["map_seed"],
            map_generator=game_output["map_generator"],
            time_played=sqlalchemy.sql.func.NOW(),
            replay_bucket=bucket_class,
        )).inserted_primary_key[0]

        # Update the participants' stats
        for user in users:
            conn.execute(model.game_participants.insert().values(
                game_id=game_id,
                user_id=user["user_id"],
                bot_id=user["bot_id"],
                version_number=user["version_number"],
                log_name=user["log_name"],
                rank=user["rank"],
                player_index=user["player_tag"],
                timed_out=user["timed_out"],
            ))

            # Increment number of games played
            conn.execute(model.bots.update().where(
                (model.bots.c.user_id == user["user_id"]) &
                (model.bots.c.id == user["bot_id"])
            ).values(
                games_played=model.bots.c.games_played + 1,
            ))

            # If this is the user's first timeout, let them know
            if user["timed_out"]:
                update_user_timeout(conn, game_id, user)

    # Update rankings
    users.sort(key=lambda user: user["rank"])
    # Set tau=0, based on discussion from Halite 1
    trueskill.setup(tau=0)
    teams = [[trueskill.Rating(mu=user["mu"], sigma=user["sigma"])]
             for user in users]
    new_ratings = trueskill.rate(teams)
    with model.engine.connect() as conn:
        for user, rating in zip(users, new_ratings):
            conn.execute(model.bots.update().where(
                (model.bots.c.user_id == user["user_id"]) &
                (model.bots.c.id == user["bot_id"]) &
                # TODO: why filter version? (Use a DB transaction?)
                (model.bots.c.version_number == user["version_number"])
            ).values(
                mu=rating[0].mu,
                sigma=rating[0].sigma,
                score=rating[0].mu - 3*rating[0].sigma,
            ))

    return response_success()


def update_user_timeout(conn, game_id, user):
    """Notify users of a timeout if applicable."""
    timed_out_count = conn.execute(sqlalchemy.sql.select([
        sqlalchemy.sql.func.count(),
    ]).select_from(model.game_participants).where(
        (model.game_participants.c.user_id == user["user_id"]) &
        (model.game_participants.c.bot_id == user["bot_id"]) &
        (model.game_participants.c.version_number ==
         user["version_number"]) &
        model.game_participants.c.timed_out
    )).first()[0]

    total_count = conn.execute(sqlalchemy.sql.select([
        model.bots.c.games_played,
    ]).select_from(model.game_participants).where(
        (model.bots.c.user_id == user["user_id"]) &
        (model.bots.c.id == user["bot_id"])
    )).first()["games_played"]

    hit_timeout_limit = timed_out_count > config.MAX_ERRORS_PER_BOT
    hit_timeout_percent = (
        total_count > 0 and
        timed_out_count / total_count > config.MAX_ERROR_PERCENTAGE
    )

    if timed_out_count == 1:
        notify.send_notification(
            user["email"],
            user["username"],
            "First bot timeout/error",
            notify.FIRST_TIMEOUT.format(
                replay_link="{}/play?game_id={}".format(
                    config.SITE_URL, game_id),
                log_link="{}/user/{}/match/{}/error_log".format(
                    config.API_URL, user["user_id"], game_id),
            ))

    elif hit_timeout_limit or hit_timeout_percent:
        # Prevent the bot from playing more games until a new bot
        # is uploaded
        conn.execute(model.bots.update().values(
            compile_status="Disabled",
        ).where((model.bots.c.user_id == user["user_id"]) &
                (model.bots.c.id == user["bot_id"])))

        notify.send_notification(
            user["email"],
            user["username"],
            "Bot timeout/error limit reached",
            notify.TIMEOUT_LIMIT.format(
                limit=config.MAX_ERRORS_PER_BOT,
                percent=int(config.MAX_ERROR_PERCENTAGE * 100),
            ))


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
    bot_restrictions = outer_bots.c.compile_status == "Successful"
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
        (model.bots.c.compile_status == "Successful") &
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
        total_players = conn.execute(model.total_ranked_bots).first()[0]
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
            model.bots.c.compile_status == "Successful"
        ).order_by(model.bots.c.games_played.asc()).limit(15).alias("least_played")

        query = least_played.select().order_by(
            sqlalchemy.sql.func.rand()
        ).limit(1)

        return conn.execute(query).first()

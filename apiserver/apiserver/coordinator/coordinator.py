import base64
import binascii
import datetime
import io
import json
import os
import random

import flask
import sqlalchemy
import trueskill

import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions

from .. import config, model, response_success, util


coordinator_api = flask.Blueprint("coordinator_api", __name__)


@coordinator_api.route("/task")
def task():
    """Serve compilation and game tasks to worker instances."""
    with model.engine.connect() as conn:
        # Check ongoing compilation tasks, and reset ones that are "stuck".
        reset_stuck_tasks = model.bots.update().where(
            (model.bots.c.compile_status == "InProgress") &
            (model.bots.c.compile_start <
             datetime.datetime.now() - datetime.timedelta(minutes=30))
        ).values(
            compile_status="Uploaded",
            compile_start=None,
        )
        conn.execute(reset_stuck_tasks)

        with conn.begin() as transaction:
            # Try to assign a compilation task.
            find_compilation_task = model.bots.select()\
                .where(model.bots.c.compile_status == "Uploaded")\
                .order_by(model.bots.c.user_id.asc())\
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

    # Try to play a game.

    # Only allow 2 or 4 player games
    player_count = 2 if random.random() > 0.5 else 4

    seed_player = None
    with model.engine.connect() as conn:
        seed_player = find_seed_player(conn)

        if seed_player:
            # Select the rest of the players
            mu_rank_limit = int(5.0 / (0.01 + random.random()) ** 0.65)

            # Find closely matched players
            sqlfunc = sqlalchemy.sql.func
            close_players = sqlalchemy.sql.select([
                model.bots.c.id.label("bot_id"),
                model.bots.c.user_id,
                model.bots.c.version_number,
                model.bots.c.mu,
                model.users.c.username,
            ]).select_from(
                model.bots.join(
                    model.users,
                    model.bots.c.user_id == model.users.c.id,
                )
            ).where(
                (model.bots.c.compile_status == "Successful") &
                (~((model.bots.c.user_id == seed_player["user_id"]) &
                   (model.bots.c.id == seed_player["bot_id"])))
            ).order_by(
                sqlalchemy.func.abs(model.bots.c.mu - seed_player["mu"])
            ).limit(mu_rank_limit).alias("muranktable")
            query = close_players.select().order_by(sqlfunc.rand())\
                .limit(player_count - 1)

            players = conn.execute(query).fetchall()
            players.insert(0, seed_player)

            # Pick map size
            map_sizes = [96, 96, 128, 128, 128, 160, 160, 160, 160, 192, 192, 192, 256]
            map_size = random.choice(map_sizes)

            players = [{
                "user_id": player["user_id"],
                "bot_id": player["bot_id"],
                "username": player["username"],
                "version_number": player["version_number"],
            } for player in players]

            if len(players) == player_count:
                return response_success({
                    "type": "game",
                    "width": map_size,
                    "height": map_size,
                    "users": players,
                })

    return response_success({
        "type": "notask",
    })


# TODO: these aren't RESTful URLs, but it's what the worker expects
@coordinator_api.route("/compile", methods=["POST"])
def update_compilation_status():
    """Update the compilation status of a bot."""
    user_id = flask.request.form.get("user_id", None)
    bot_id = flask.request.form.get("bot_id", None)
    did_compile = flask.request.form.get("did_compile", False)
    language = flask.request.form.get("language", "Other")

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
            # TODO: email the user

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
                        # TODO:
                        # lastRank=user["rank"] or 0,
                        last_rank=0,
                        last_score=bot["score"],
                        last_num_players=num_active_users,
                        last_games_played=bot["games_played"],
                        language=bot["language"],
                    )
                )

            conn.execute(
                model.bots.update().where(
                    (model.bots.c.user_id == user_id) &
                    (model.bots.c.id == bot_id)
                ).values(
                    language=language,
                    version_number=model.bots.c.version_number + 1,
                    games_played=0,
                    mu=25.000,
                    sigma=8.333,
                    score=0,
                )
            )
            return response_success()
        else:
            # TODO: email the user
            return response_success()


@coordinator_api.route("/botFile", methods=["POST"])
def upload_bot():
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
    if ("game_output" not in flask.request.values or
            "users" not in flask.request.values):
        raise util.APIError(
            400, message="Please provide both the game output and users.")

    game_output = json.loads(flask.request.values["game_output"])
    users = json.loads(flask.request.values["users"])

    with model.engine.connect() as conn:
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

    # Store the replay and any error logs
    replay_name = os.path.basename(game_output["replay"])
    replay_key, _ = os.path.splitext(replay_name)
    if replay_name not in flask.request.files:
        raise util.APIError(
            400, message="Replay file not found in uploaded files.")
    blob = gcloud_storage.Blob(replay_key, model.get_replay_bucket(),
                               chunk_size=262144)
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
        )).inserted_primary_key

    # Update the participants' stats
    with model.engine.connect() as conn:
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

    # TODO: send first game email, first timeout email

    # Update rankings
    users.sort(key=lambda user: user["rank"])
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


def find_seed_player(conn):
    """
    Find a seed player for a game.
    :param conn: A database connection.
    :return: A seed player, or None.
    """
    sqlfunc = sqlalchemy.sql.func
    seed_player = None
    if not config.COMPETITION_FINALS_PAIRING:
        rand_value = random.random()

        if rand_value > 0.5:
            ordering = sqlfunc.rand() * -sqlfunc.pow(model.bots.c.sigma, 2)
            result = conn.execute(sqlalchemy.sql.select([
                model.users.c.id.label("user_id"),
                model.bots.c.id.label("bot_id"),
                model.users.c.username,
                model.bots.c.version_number,
                model.bots.c.mu,
            ]).select_from(
                model.users.join(
                    model.bots,
                    model.users.c.id == model.bots.c.user_id
                )
            ).where(
                (model.bots.c.compile_status == "Successful") &
                (model.bots.c.games_played < 400)
            ).order_by(ordering).limit(1).reduce_columns()).first()
            if result:
                seed_player = result
        elif 0.25 < rand_value <= 0.5:
            # Find the users in the most recent games, and pick a seed player
            # from those
            max_time = sqlfunc.max(model.games.c.time_played).label("max_time")
            user_id = model.game_participants.c.user_id
            bot_id = model.game_participants.c.bot_id

            recent_gamers = sqlalchemy.sql.select([
                max_time,
                user_id,
                bot_id,
                model.users.c.username,
                model.bots.c.version_number,
                model.bots.c.mu,
            ]).select_from(
                model.game_participants.join(
                    model.games,
                    model.games.c.id == model.game_participants.c.game_id,
                ).join(
                    model.users,
                    model.users.c.id == model.game_participants.c.user_id,
                ).join(
                    model.bots,
                    (model.bots.c.id == model.game_participants.c.bot_id) &
                    (model.bots.c.user_id == model.game_participants.c.user_id)
                )
            ).group_by(user_id, bot_id).reduce_columns().alias("temptable")

            # Of those users, select ones with under 400 games, preferring
            # ones in older games, and get their data
            outer_bots = model.bots.alias("u")
            potential_players = sqlalchemy.sql.select([
                recent_gamers.c.user_id,
                recent_gamers.c.bot_id,
                recent_gamers.c.username,
                recent_gamers.c.version_number,
                recent_gamers.c.mu,
            ]).select_from(
                recent_gamers.join(
                    outer_bots,
                    outer_bots.c.id == recent_gamers.c.user_id)) \
                .where((outer_bots.c.games_played < 400) &
                       (outer_bots.c.compile_status == "Successful")) \
                .order_by(recent_gamers.c.max_time.asc()) \
                .limit(15).alias("orderedtable")

            # Then sort them randomly and take one
            query = potential_players.select().order_by(sqlfunc.rand()).limit(1)
            result = conn.execute(query).first()
            if result:
                seed_player = result

        if rand_value <= 0.25 or not seed_player:
            # Same as the previous case, but we do not restrict how many
            # games the user can have played, and we do not sort randomly.
            max_time = sqlfunc.max(model.games.c.time_played).label("max_time")
            user_id = model.game_participants.c.user_id
            bot_id = model.game_participants.c.bot_id

            recent_gamers = sqlalchemy.sql.select([
                max_time,
                user_id,
                bot_id,
                model.users.c.username,
                model.bots.c.version_number,
                model.bots.c.mu,
            ]).select_from(
                model.game_participants.join(
                    model.games,
                    model.games.c.id == model.game_participants.c.game_id,
                ).join(
                    model.users,
                    model.users.c.id == model.game_participants.c.user_id,
                ).join(
                    model.bots,
                    (model.bots.c.id == model.game_participants.c.bot_id) &
                    (model.bots.c.user_id == model.game_participants.c.user_id)
                )
            ).group_by(user_id, bot_id).reduce_columns().alias("temptable")

            outer_bots = model.bots.alias("u")
            potential_players = sqlalchemy.sql.select([
                recent_gamers.c.user_id,
                recent_gamers.c.bot_id,
                recent_gamers.c.username,
                recent_gamers.c.version_number,
                recent_gamers.c.mu,
            ]).select_from(
                recent_gamers.join(
                    outer_bots,
                    outer_bots.c.id == recent_gamers.c.user_id)) \
                .where(outer_bots.c.compile_status == "Successful") \
                .order_by(recent_gamers.c.max_time.asc()) \
                .limit(15).alias("orderedtable")

            query = potential_players.select().limit(1)
            result = conn.execute(query).first()
            if result:
                seed_player = result
    else:
        # TODO: wtf??
        pass

    return seed_player

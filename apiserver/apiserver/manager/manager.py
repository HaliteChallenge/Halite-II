import base64
import binascii
import functools
import io
import json
import os
import random

import flask
import sqlalchemy
import trueskill

import google.cloud.storage as gcloud_storage

from .. import config, model, response_success, response_failure


manager_api = flask.Blueprint("manager_api", __name__)


def requires_valid_worker(view):
    """
    Decorator that checks that the remote client is a valid worker.

    A valid worker must provide an API key.

    API keys are generated and stored when the worker is initially started.
    """
    @functools.wraps(view)
    def _requires_valid_worker(*args, **kwargs):
        api_key = flask.request.values.get("apiKey", None)
        if api_key is None:
            return flask.abort(401)

        with model.engine.connect() as conn:
            find_worker = model.workers\
                .select(model.workers.c.apiKey)\
                .where(model.workers.c.apiKey == api_key)
            if len(conn.execute(find_worker).fetchall()) != 1:
                return flask.abort(401)

        kwargs["api_key"] = api_key
        return view(*args, **kwargs)

    return _requires_valid_worker


@manager_api.route("/task")
@requires_valid_worker
def task(*, api_key):
    """Serve compilation and game tasks to worker instances."""
    with model.engine.connect() as conn:
        # Try to assign a compilation task.
        find_compilation_task = model.users\
            .select(model.users.c.userID)\
            .where(model.users.c.compileStatus == 1)\
            .order_by(sqlalchemy.asc(model.users.c.userID))\
            .limit(1)
        users = conn.execute(find_compilation_task).fetchall()
        if users:
            user_id = users[0]["userID"]
            # TODO: some way to clear compileStatus if it gets stuck
            update = model.users.update() \
                .where(model.users.c.userID == user_id) \
                .values(compileStatus=2)
            conn.execute(update)
            return response_success({
                "type": "compile",
                "user": user_id,
            })

    # Try to play a game.
    # Need user: ID, username, and # of submissions.
    # TODO: make sure these end up in JSON as keys
    def desired_columns_of(table):
        return [
            table.c.userID,
            table.c.username,
            table.c.numSubmissions,
            table.c.mu,
        ]
    # Only allow 2 or 4 player games
    player_count = 2 if random.random() > 0.5 else 4
    seed_player = None
    with model.engine.connect() as conn:
        seed_player = find_seed_player(conn, desired_columns_of)

        if seed_player:
            # Select the rest of the players
            mu_rank_limit = int(5.0 / (0.01 + random.random()) ** 0.65)

            # Find closely matched players
            sqlfunc = sqlalchemy.sql.func
            close_players = sqlalchemy.sql.select(
                desired_columns_of(model.users)
            ).where(
                (model.users.c.isRunning == 1) &
                (model.users.c.userID != seed_player["userID"])
            ).order_by(
                sqlalchemy.func.abs(model.users.c.mu - seed_player["mu"])
            ).limit(mu_rank_limit).alias("muranktable")
            query = close_players.select().order_by(sqlfunc.rand())\
                .limit(player_count - 1)

            players = conn.execute(query).fetchall()
            players.insert(0, seed_player)

            # Pick map size
            map_sizes = [96, 96, 128, 128, 128, 160, 160, 160, 160, 192, 192, 192, 256]
            map_size = random.choice(map_sizes)

            players = [{
                "userID": player["userID"],
                "username": player["username"],
                "numSubmissions": player["numSubmissions"],
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
@manager_api.route("/compile", methods=["POST"])
@requires_valid_worker
def update_compilation_status(*, api_key):
    """Update the compilation status of a bot."""
    user_id = flask.request.form.get("userID", None)
    did_compile = flask.request.form.get("didCompile", False)

    if user_id is None:
        return response_failure("Must provide user ID.")

    language = flask.request.form.get("language", "Other")

    # Increment the number of compilation tasks this worker has completed
    # TODO: this field is never actually used
    with model.engine.connect() as conn:
        update_worker = model.workers.update()\
            .where(model.workers.c.apiKey == api_key)\
            .values(numCompiles=model.workers.c.numCompiles + 1)
        conn.execute(update_worker)

        update = model.users.update()\
            .where(model.users.c.userID == user_id)\
            .values(compileStatus=0)
        conn.execute(update)

        if did_compile:
            # TODO: email the user

            user = model.users.select().where(model.users.c.userID == user_id)
            user = conn.execute(user).first()

            # This is backwards of the order in the original PHP, but the original
            # PHP updated the table using the -old- values of the User row. This
            # ordering makes it clearer that this is intentional.
            if user["numSubmissions"] != 0:
                # TODO: make this more efficient
                num_active_users = len(conn.execute(
                    model.users
                        .select(model.users.c.userID)
                        .where(model.users.c.isRunning == 1)
                ).fetchall())

                conn.execute(
                    model.user_history.insert().values(
                        userID=user_id,
                        versionNumber=user["numSubmissions"],
                        # TODO: figure out why this isn't defined
                        lastRank=user["rank"] or 0,
                        lastNumPlayers=num_active_users,
                        lastNumGames=user["numGames"],
                    )
                )

            update = model.users.update()\
                .where(model.users.c.userID == user_id)\
                .values(
                    numSubmissions=model.users.c.numSubmissions + 1,
                    numGames=0,
                    mu=25.000,
                    sigma=8.333,
                    compileStatus=0,
                    isRunning=1,
                    language=language,
            )
            conn.execute(update)
            return response_success()
        else:
            # TODO: email the user
            return response_success()


@manager_api.route("/botFile", methods=["POST"])
@requires_valid_worker
def upload_bot(*, api_key):
    user_id = flask.request.form.get("userID", None)

    if "bot.zip" not in flask.request.files:
        return response_failure("Please provide the bot file.")

    uploaded_file = flask.request.files["bot.zip"]
    # Save to GCloud
    blob = gcloud_storage.Blob(str(user_id), model.get_bot_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)
    return response_success()


@manager_api.route("/botFile", methods=["GET"])
@requires_valid_worker
def download_bot(*, api_key):
    user_id = flask.request.values.get("userID", None)
    compile = flask.request.values.get("compile", False)

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    # Retrieve from GCloud
    blob = gcloud_storage.Blob(str(user_id), bucket, chunk_size=262144)
    buffer = io.BytesIO()
    blob.download_to_file(buffer)
    buffer.seek(0)
    return flask.send_file(buffer, mimetype="application/zip",
                           as_attachment=True,
                           attachment_filename=str(user_id)+".zip")


@manager_api.route("/botHash")
@requires_valid_worker
def hash_bot(*, api_key):
    """Get the MD5 hash of a compiled bot."""
    user_id = flask.request.args.get("userID", None)
    compile = flask.request.args.get("compile", False)

    if not user_id:
        return response_failure("Please provide the user ID.")

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    blob = bucket.get_blob(str(user_id))
    if blob is None:
        return response_failure("Bot does not exist.")

    return response_success({
        "hash": binascii.hexlify(base64.b64decode(blob.md5_hash)).decode('utf-8'),
    })


@manager_api.route("/game", methods=["POST"])
@requires_valid_worker
def upload_game(*, api_key):
    if "game_output" not in flask.request.values \
            or "users" not in flask.request.values:
        return response_failure(
            "Please provide both the game output and users.")

    game_output = json.loads(flask.request.values["game_output"])
    users = json.loads(flask.request.values["users"])

    # Increment the number of games this worker has handled
    # TODO: why do we even track this?
    with model.engine.connect() as conn:
        conn.execute(model.workers.update().where(
            model.workers.c.apiKey == api_key
        ).values(
            numGames=model.workers.c.numGames + 1,
        ))

        worker_id = conn.execute(sqlalchemy.sql.select([
            model.workers.c.workerID
        ]).where(model.workers.c.apiKey == api_key)).first()["workerID"]

    # If the user has submitted a new bot in the meanwhile, ignore the game
    with model.engine.connect() as conn:
        for user in users:
            stored_user = conn.execute(
                sqlalchemy.sql.select([
                    model.users.c.userID,
                    model.users.c.onEmailList,
                    model.users.c.email,
                    model.users.c.numSubmissions,
                    model.users.c.mu,
                    model.users.c.sigma,
                ]).where(model.users.c.userID == user["userID"])
            ).first()

            if stored_user["numSubmissions"] != user["numSubmissions"]:
                return response_success({
                    "message":
                        "User {} has uploaded a new bot, discarding match."\
                            .format(user["userID"])
                })

            user.update(dict(stored_user))

    # Store the replay and any error logs
    replay_name = os.path.basename(game_output["replay"])
    replay_key, _ = os.path.splitext(replay_name)
    if replay_name not in flask.request.files:
        return response_failure("Replay file not found in uploaded files.")
    blob = gcloud_storage.Blob(replay_key, model.get_replay_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(flask.request.files[replay_name])

    error_logs = {}
    for user in users:
        if user["didTimeout"]:
            error_log_name = user["errorLogName"]
            if error_log_name not in flask.request.files:
                return response_failure(
                    "Error log {} not found in uploaded files."\
                        .format(error_log_name))

            blob = gcloud_storage.Blob(os.path.basename(error_log_name),
                                       model.get_error_log_bucket(),
                                       chunk_size=262144)
            blob.upload_from_file(flask.request.files[error_log_name])

    # TODO: the original code deletes games if there are over 600k in the
    # database. Is that really a concern for us?

    # Store game results in database
    with model.engine.connect() as conn:
        game_id = conn.execute(model.games.insert().values(
            replayName=replay_key,
            mapWidth=game_output["map_width"],
            mapHeight=game_output["map_height"],
            # TODO: store map seed and algorithm
            timestamp=sqlalchemy.sql.func.NOW(),
            workerID=worker_id,
        )).inserted_primary_key

    # Update the participants' stats
    with model.engine.connect() as conn:
        for user in users:
            timeout = 1 if user["didTimeout"] else 0
            conn.execute(model.gameusers.insert().values(
                gameID=game_id,
                userID=user["userID"],
                errorLogName=user["errorLogName"],
                rank=user["rank"],
                playerIndex=user["playerTag"],
                didTimeout=timeout,
                versionNumber=user["numSubmissions"]
            ))

            # Increment number of games played
            conn.execute(model.users.update().where(
                model.users.c.userID == user["userID"]
            ).values(
                numGames=model.users.c.numGames + 1,
            ))

    # TODO: send first game email, first timeout email

    # Update rankings
    users.sort(key=lambda user: user["rank"], reverse=True)
    teams = [[trueskill.Rating(mu=user["mu"], sigma=user["sigma"])]
             for user in users]
    new_ratings = trueskill.rate(teams)
    with model.engine.connect() as conn:
        for user, rating in zip(users, new_ratings):
            conn.execute(model.users.update().where(
                (model.users.c.userID == user["userID"]) &
                # TODO: why filter on numSubmissions? (Should we be using a DB transaction?)
                (model.users.c.numSubmissions == user["numSubmissions"])
            ).values(
                mu=rating[0].mu,
                sigma=rating[0].sigma,
            ))

    # Update everyone's overall rank
    # TODO: is there a more efficient way to have the DB implement this?

    # TODO:
    # with model.engine.connect() as conn:
    #     all_users = conn.execute(sqlalchemy.sql.select([
    #         model.users.c.mu,
    #         model.users.c.sigma,
    #     ]).where(model.users.c.isRunning == 1)).fetchall()
    #
    #     all_users.sort(key=lambda user: user["mu"] - 3 * user["sigma"])

    return response_success()


def find_seed_player(conn, desired_columns_of):
    """
    Find a seed player for a game.
    :param conn: A database connection.
    :param desired_columns_of: Function that returns a list of columns desired
    from a given table of user data.
    :return: A seed player, or None.
    """
    sqlfunc = sqlalchemy.sql.func
    seed_player = None
    if not config.COMPETITION_FINALS_PAIRING:
        rand_value = random.random()

        if rand_value > 0.5:
            ordering = sqlfunc.rand() * -sqlfunc.pow(model.users.c.sigma, 2)
            query = sqlalchemy.sql.select(desired_columns_of(model.users))\
                .where(model.users.c.isRunning == 1 and
                       model.users.c.numGames < 400)\
                .order_by(ordering)\
                .limit(1)
            result = conn.execute(query).fetchall()
            if result:
                seed_player = result[0]
        elif 0.25 < rand_value <= 0.5:
            # Find the users in the most recent games, and pick a seed player
            # from those
            games = model.games
            gameusers = model.gameusers

            max_time = sqlfunc.max(games.c.timestamp).label("maxTime")
            user_id = gameusers.c.userID.label("userID")
            recent_gamers = sqlalchemy.sql.select([
                max_time,
                user_id
            ]).select_from(
                gameusers.join(games, games.c.gameID == gameusers.c.gameID)
            ).group_by(user_id).alias("temptable")

            # Of those users, select ones with under 400 games, preferring
            # ones in older games, and get their data
            outer_users = model.users.alias("u")
            potential_players = sqlalchemy.sql.select(
                desired_columns_of(outer_users)
            ).select_from(
                recent_gamers.join(
                    outer_users,
                    outer_users.c.userID == recent_gamers.c.userID)) \
                .where((outer_users.c.numGames < 400) &
                       (outer_users.c.isRunning == 1)) \
                .order_by(recent_gamers.c.maxTime.asc()) \
                .limit(15).alias("orderedtable")

            # Then sort them randomly and take one
            query = sqlalchemy.sql.select(
                desired_columns_of(potential_players)
            ).order_by(sqlfunc.rand()).limit(1)
            result = conn.execute(query).fetchall()
            if result:
                seed_player = result[0]

        if rand_value <= 0.25 or not seed_player:
            # Same as the previous case, but we do not restrict how many
            # games the user can have played, and we do not sort randomly.
            games = model.games
            gameusers = model.gameusers

            max_time = sqlfunc.max(games.c.timestamp).label("maxTime")
            user_id = gameusers.c.userID.label("userID")
            recent_gamers = sqlalchemy.sql.select([
                max_time,
                user_id
            ]).select_from(
                gameusers.join(games, games.c.gameID == gameusers.c.gameID)
            ).group_by(user_id).alias("temptable")

            # Of those users, select ones with under 400 games, preferring
            # ones in older games, and get their data
            outer_users = model.users.alias("u")
            potential_players = sqlalchemy.sql.select(
                desired_columns_of(outer_users)
            ).select_from(
                recent_gamers.join(
                    outer_users,
                    outer_users.c.userID == recent_gamers.c.userID)) \
                .where(outer_users.c.isRunning == 1) \
                .order_by(recent_gamers.c.maxTime.asc()) \
                .limit(1)

            result = conn.execute(potential_players).fetchall()
            if result:
                seed_player = result[0]
    else:
        pass

    return seed_player

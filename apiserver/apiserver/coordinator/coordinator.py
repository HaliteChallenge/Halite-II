import json
import os

import flask
import sqlalchemy
import sqlalchemy.exc
import trueskill

import google.cloud.storage as gcloud_storage

from .. import config, model, notify, response_success, util

from .blueprint import coordinator_api
from .compilation import serve_compilation_task, reset_compilation_tasks
from .matchmaking import serve_game_task


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
            response = serve_compilation_task(conn)
            if response:
                return response

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
        total_users = conn.execute(model.total_ranked_users).first()[0]
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
                    model.bots.c.language,
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
                user["tier"] = util.tier(stored_rank["rank"], total_users)
            else:
                user["tier"] = util.tier(total_users, total_users)

    # Store the replay and any error logs
    replay_name = os.path.basename(game_output["replay"])
    replay_key, bucket_class = store_game_artifacts(replay_name, users)

    # Store game results in database
    store_game_results(game_output, replay_key, bucket_class, users)
    # Update rankings
    update_rankings(users)

    return response_success()


def store_game_artifacts(replay_name, users):
    """
    Upload the replay and any error logs to object storage.

    `users` should be a list of user objects with the user ID, a flag to
    indicate timeout, and the filename of the error log.

    Returns the key of the replay in object storage and the bucket the replays
    were saved in.
    """
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

    # Store error logs
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

    return replay_key, bucket_class


def store_game_results(game_output, replay_key, bucket_class, users):
    """
    Store the outcome of a game in the database.

    :param game_output: The JSON output of the Halite game environment.
    :param replay_key: The key of the replay file in object storage.
    :param bucket_class: Which bucket the replay was stored in.
    :param users: The list of user objects for this game.
    :return:
    """
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
                # Which player in the game (numbered starting from 0) was
                # this user?
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


def update_rankings(users):
    """
    Update the rankings via TrueSkill and store in the database.

    `users` should be a list of user objects with the game rank (which place
    the user got in the game), the bot's mu and sigma, the user and bot IDs,
    and the bot version number.
    """
    users.sort(key=lambda user: user["rank"])
    # Set tau=0, based on discussion from Halite 1
    trueskill.setup(tau=0)
    teams = [[trueskill.Rating(mu=user["mu"], sigma=user["sigma"])]
             for user in users]
    new_ratings = trueskill.rate(teams)
    with model.engine.connect() as conn:
        for user, rating in zip(users, new_ratings):
            new_score = rating[0].mu - 3*rating[0].sigma
            conn.execute(model.bots.update().where(
                (model.bots.c.user_id == user["user_id"]) &
                (model.bots.c.id == user["bot_id"]) &
                # Filter on version so we don't update the score for an old
                # version of the bot
                (model.bots.c.version_number == user["version_number"])
            ).values(
                mu=rating[0].mu,
                sigma=rating[0].sigma,
                score=new_score,
            ))

            # Update the hackathon scoring tables
            hackathons = conn.execute(sqlalchemy.sql.select([
                model.hackathons.c.id.label("hackathon_id"),
            ]).select_from(
                model.hackathon_participants.join(
                    model.hackathons,
                    (model.hackathon_participants.c.hackathon_id == model.hackathons.c.id) &
                    (model.hackathon_participants.c.user_id == user["user_id"])
                )
            ).where(
                (model.hackathons.c.start_date <= sqlalchemy.sql.func.now()) &
                (model.hackathons.c.end_date > sqlalchemy.sql.func.now())
            ))

            for hackathon in hackathons.fetchall():
                hackathon_id = hackathon["hackathon_id"]
                try:
                    # Try and insert
                    insert_values = {
                        "hackathon_id": hackathon_id,
                        "user_id": user["user_id"],
                        "bot_id": user["bot_id"],
                        "score": new_score,
                        "mu": rating[0].mu,
                        "sigma": rating[0].sigma,
                        "version_number": user["version_number"],
                        "language": user["language"],
                    }

                    conn.execute(
                        model.hackathon_snapshot.insert().values(
                            **insert_values))
                except sqlalchemy.exc.IntegrityError:
                    # Row exists, update
                    condition = ((model.hackathon_snapshot.c.hackathon_id ==
                                  hackathon_id) &
                                 (model.hackathon_snapshot.c.user_id ==
                                  user["user_id"]) &
                                 (model.hackathon_snapshot.c.bot_id ==
                                  user["bot_id"]))
                    conn.execute(
                        model.hackathon_snapshot.update().values(
                            score=new_score,
                            mu=rating[0].mu,
                            sigma=rating[0].sigma,
                            version_number=user["version_number"],
                            language=user["language"],
                            games_played=model.hackathon_snapshot.c.games_played + 1,
                        ).where(condition))


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
            compile_status=model.CompileStatus.DISABLED.value,
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


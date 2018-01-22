import json
import os

import flask
import sqlalchemy
import sqlalchemy.exc
import trueskill
import zstd

import google.cloud.storage as gcloud_storage

from .. import config, model, notify, util

from .blueprint import coordinator_api
from .compilation import serve_compilation_task, reset_compilation_tasks
from .matchmaking import serve_game_task
from .stat import GameStat


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

        if config.COMPETITION_FINALS_PAIRING or config.COMPETITION_OPEN:
            # Otherwise, play a game
            # If the worker has a GPU, try really hard to give it some work to do
            tries = 0
            while tries == 0 or ((has_gpu or config.COMPETITION_FINALS_PAIRING)
                                 and tries < 10):
                response = serve_game_task(conn, has_gpu=has_gpu)
                if response:
                    return response
                tries += 1

    return util.response_success({
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
    challenge = json.loads(flask.request.values.get("challenge", "null"))

    replay_name = os.path.basename(game_output["replay"])
    if replay_name not in flask.request.files:
        raise util.APIError(
            400, message="Replay file not found in uploaded files.")

    stats = parse_replay(decode_replay(flask.request.files[replay_name]))
    if stats is None:
        raise util.APIError(
            400, message="Replay file cannot be parsed.")

    # Store the replay and any error logs
    replay_key, bucket_class = store_game_artifacts(replay_name, users)

    with model.engine.begin() as conn:
        total_users = conn.execute(model.total_ranked_users).first()[0]
        # Sort the users to prevent deadlock in the stored_bot for update lock
        for user in sorted(users, key=lambda x: x['user_id']):
            stored_user = conn.execute(
                sqlalchemy.sql.select([
                    model.users.c.id.label("user_id"),
                    model.users.c.on_email_list,
                    model.users.c.github_email.label("email"),
                    model.users.c.player_level,
                    model.users.c.creation_time,
                    model.users.c.username,
                    model.organizations.c.organization_name,
                ]).select_from(model.users.join(
                    model.organizations,
                    model.organizations.c.id == model.users.c.organization_id,
                    isouter=True
                )).where(model.users.c.id == user["user_id"])
            ).first()

            stored_bot = conn.execute(
                sqlalchemy.sql.select([
                    model.bots.c.version_number,
                    model.bots.c.language,
                    model.bots.c.mu,
                    model.bots.c.sigma,
                ], for_update=True).where(
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
                return util.response_success({
                    "message": "User {} has uploaded a new bot, discarding "
                               "match.".format(user["user_id"])
                })

            user.update(dict(stored_user))
            user.update(dict(stored_bot))
            if stored_rank:
                user["leaderboard_rank"] = stored_rank["rank"]
                user["tier"] = util.tier(stored_rank["rank"], total_users)
            else:
                user["leaderboard_rank"] = total_users
                user["tier"] = util.tier(total_users, total_users)

        # Store game results in database
        game_id = store_game_results(conn, game_output, stats,
                                     replay_key, bucket_class,
                                     users, challenge)
        # Store game stats in database
        store_game_stats(conn, game_output, stats, game_id, users)
        # Update rankings
        if not challenge:
            update_rankings(conn, users)

    return util.response_success()


def store_game_artifacts(replay_name, users):
    """
    Upload the replay and any error logs to object storage.

    `users` should be a list of user objects with the user ID, a flag to
    indicate timeout, and the filename of the error log.

    Returns the key of the replay in object storage and the bucket the replays
    were saved in.
    """
    replay_key, _ = os.path.splitext(replay_name)

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


def store_game_results(conn, game_output, stats, replay_key, bucket_class,
                       users, challenge):
    """
    Store the outcome of a game in the database.

    :param game_output: The JSON output of the Halite game environment.
    :param replay_key: The key of the replay file in object storage.
    :param bucket_class: Which bucket the replay was stored in.
    :param users: The list of user objects for this game.
    :param challenge: The challenge ID for this game, or None.
    :return game_id: ID of the record in game table
    """

    # Store game results in database
    game_id = conn.execute(model.games.insert().values(
        replay_name=replay_key,
        map_width=game_output["map_width"],
        map_height=game_output["map_height"],
        map_seed=game_output["map_seed"],
        map_generator=game_output["map_generator"],
        time_played=sqlalchemy.sql.func.NOW(),
        replay_bucket=bucket_class,
        challenge_id=challenge,
    )).inserted_primary_key[0]

    # Initialize the game view stats
    conn.execute(model.game_view_stats.insert().values(
        game_id=game_id,
        views_total=0,
    ))

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
            leaderboard_rank=user["leaderboard_rank"],
            mu=user["mu"],
            sigma=user["sigma"]
        ))

        if not challenge:
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

    if challenge is not None:
        store_challenge_results(conn, users, challenge, stats)

    return game_id


def store_challenge_results(conn, users, challenge, stats):
    conn.execute(model.challenges.update().values(
        num_games=model.challenges.c.num_games + 1,
        status=sqlalchemy.case(
            [
                (model.challenges.c.num_games >= 30,
                 model.ChallengeStatus.FINISHED.value),
            ],
            else_=model.ChallengeStatus.CREATED.value,
        ),
    ).where(model.challenges.c.id == challenge))

    for user in users:
        # 4 points for 1st place, 3 for 2nd, etc.
        points = len(users) - user["rank"] + 1
        if len(users) == 2:
            points += 2

        if user["player_tag"] in stats.players:
            ships_produced = stats.players[user["player_tag"]].ships_produced
            attacks_made = stats.players[user["player_tag"]].attacks_total
        else:
            ships_produced = attacks_made = 0

        conn.execute(model.challenge_participants.update().values(
            points=model.challenge_participants.c.points + points,
            ships_produced=model.challenge_participants.c.ships_produced + ships_produced,
            attacks_made=model.challenge_participants.c.attacks_made + attacks_made,
        ).where((model.challenge_participants.c.challenge_id == challenge) &
                (model.challenge_participants.c.user_id == user["user_id"])))

    challenge_row = conn.execute(
        model.challenges.select(model.challenges.c.id == challenge)
    ).first()

    if challenge_row and \
       challenge_row["status"] == model.ChallengeStatus.FINISHED.value:
        winner = conn.execute(model.challenge_participants.select(
            model.challenge_participants.c.challenge_id == challenge
        ).order_by(
            model.challenge_participants.c.points.desc(),
            model.challenge_participants.c.ships_produced.desc(),
            model.challenge_participants.c.attacks_made.desc(),
        )).first()
        conn.execute(model.challenges.update().values(
            finished=sqlalchemy.sql.func.now(),
            winner=winner["user_id"],
        ).where(model.challenges.c.id == challenge))


def store_game_stats(conn, game_output, stats, game_id, users):
    """
    Store additional game stats into database.

    :param game_output: The JSON output of the Halite game environment.
    :param game_id: ID of the current match in game table
    :param users: The list of user objects for this game.
    :return:
    """
    # Store game stats in database
    conn.execute(model.game_stats.insert().values(
        game_id=game_id,
        turns_total=stats.turns_total,
        planets_destroyed=stats.planets_destroyed,
        ships_produced=stats.ships_produced,
        ships_destroyed=stats.ships_destroyed
    ))

    # Use player_tag to get the correct user_id from replay
    for user in users:
        player_tag = user["player_tag"]
        if player_tag in stats.players:
            conn.execute(model.game_bot_stats.insert().values(
                game_id=game_id,
                user_id=user["user_id"],
                bot_id=user["bot_id"],
                planets_controlled=stats.players[player_tag].planets_controlled,
                ships_produced=stats.players[player_tag].ships_produced,
                ships_alive=stats.players[player_tag].ships_alive,
                ships_alive_ratio=stats.players[player_tag].ships_alive_ratio,
                ships_relative_ratio=stats.players[player_tag].ships_relative_ratio,
                planets_destroyed=stats.players[player_tag].planets_destroyed,
                attacks_total=stats.players[player_tag].attacks_total
            ))


def decode_replay(replay_file_obj):
    """
    Parse replay file into JSON format.
    Current replay file format is Zstandard

    :param replay_file_obj: The replay file (flask.FileStorage).
    :return: JSON output
    """
    decoder = zstd.ZstdDecompressor()
    # Rewind to the beginning of the file obj, because
    # gcloud might have read it first
    replay_file_obj.seek(0)
    replay_data = replay_file_obj.read()
    try:
        decoded_data = decoder.decompress(replay_data)
        json_data = json.loads(decoded_data.decode('utf-8').strip())
        return json_data
    except zstd.ZstdError:
        # The replay file can't be decoded.
        return None
    finally:
        # Seek the replay file back to start so we can upload it.
        replay_file_obj.seek(0)


def parse_replay(replay):
    """
    Read replay (turn by turn) and compute stats for a match.

    :param replay: Decoded replay data
    :return: Interesting stats to put into database
    """
    if replay is None:
        return None

    stats = GameStat(replay["num_players"])
    stats.turns_total = len(replay['frames']) - 1
    for frame in replay["frames"]:
        for event in frame.get("events", []):
            player_tag = event["entity"].get("owner")
            if event["event"] == "spawned":
                stats.ships_produced += 1
                stats.players[player_tag].ships_produced += 1
            elif event["event"] == "destroyed":
                if event["entity"]["type"] == "ship":
                    stats.ships_destroyed += 1
                elif event["entity"]["type"] == "planet":
                    stats.planets_destroyed += 1
                    if player_tag:
                        stats.players[player_tag].planets_destroyed += 1
            elif event["event"] == "attack":
                stats.players[player_tag].attacks_total += 1

    ships_alive_total = sum([len(ships) for ships in replay["frames"][-1]["ships"].values()])
    for player_tag in stats.players.keys():
        stats.players[player_tag].ships_alive = len(replay["frames"][-1]["ships"][str(player_tag)])
        # use max(1.0, ...) to avoid ZeroDivisionError
        stats.players[player_tag].ships_alive_ratio = 1.0 * stats.players[player_tag].ships_alive / max(1.0, stats.players[player_tag].ships_produced)
        stats.players[player_tag].ships_relative_ratio = 1.0 * stats.players[player_tag].ships_alive / max(1.0, ships_alive_total)

    for planet in replay["frames"][-1]["planets"].values():
        if planet["owner"] is not None:
            stats.players[planet["owner"]].planets_controlled += 1

    return stats


def update_rankings(conn, users):
    """
    Update the rankings via TrueSkill and store in the database.

    `users` should be a list of user objects with the game rank (which place
    the user got in the game), the bot's mu and sigma, the user and bot IDs,
    and the bot version number.
    """
    users.sort(key=lambda user: user["rank"])
    # Set tau and draw_probability to more reasonable values than the defaults
    if config.COMPETITION_FINALS_PAIRING:
        trueskill.setup(tau=0.0, draw_probability=0.001)
    else:
        trueskill.setup(tau=0.008, draw_probability=0.001)
    teams = [[trueskill.Rating(mu=user["mu"], sigma=user["sigma"])]
             for user in users]
    new_ratings = trueskill.rate(teams)
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
                    "games_played": 1,
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

    bot = conn.execute(sqlalchemy.sql.select([
        model.bots.c.games_played,
        model.bots.c.timeout_sent,
    ]).select_from(model.bots).where(
        (model.bots.c.user_id == user["user_id"]) &
        (model.bots.c.id == user["bot_id"])
    )).first()

    if not bot:
        return

    total_count = bot["games_played"]
    timeout_sent = bot["timeout_sent"]

    # Disable bots that error out too much
    hit_timeout_limit = timed_out_count > config.MAX_ERRORS_PER_BOT
    # Disable bots that error in too great a percentage of their games
    hit_timeout_percent = (
        total_count > 10 and
        timed_out_count / total_count > config.MAX_ERROR_PERCENTAGE
    )

    recipient = notify.Recipient(user["user_id"], user["username"],
                                 user["email"],
                                 user["organization_name"],
                                 user["player_level"],
                                 user["creation_time"])
    if timed_out_count == 1 and not timeout_sent:
        notify.send_templated_notification(
            recipient,
            config.FIRST_TIMEOUT_TEMPLATE,
            {
                "limit": config.MAX_ERRORS_PER_BOT,
                "percent": int(config.MAX_ERROR_PERCENTAGE * 100),
                "game_id": game_id,
                "user_id": user["user_id"],
                "bot_id": user["bot_id"],
                "replay_link": util.build_site_url("/play", {
                    "game_id": game_id
                }),
                "log_link": "{}/user/{}/match/{}/error_log".format(
                    config.API_URL, user["user_id"], game_id),
            },
            config.GAME_ERROR_MESSAGES,
            config.C_BOT_TIMED_OUT)

        conn.execute(model.bots.update().values(timeout_sent=True).where(
            (model.bots.c.user_id == user["user_id"]) &
            (model.bots.c.id == user["bot_id"])
        ))

    elif hit_timeout_limit or hit_timeout_percent:
        # For now, do not disable bots
        return
        # Prevent the bot from playing more games until a new bot
        # is uploaded
        conn.execute(model.bots.update().values(
            compile_status=model.CompileStatus.DISABLED.value,
        ).where((model.bots.c.user_id == user["user_id"]) &
                (model.bots.c.id == user["bot_id"])))

        notify.send_templated_notification(
            recipient,
            config.BOT_DISABLED_TEMPLATE,
            {
                "limit": config.MAX_ERRORS_PER_BOT,
                "percent": int(config.MAX_ERROR_PERCENTAGE * 100),
                "game_id": game_id,
                "user_id": user["user_id"],
                "bot_id": user["bot_id"],
                "replay_link": util.build_site_url("/play", {
                    "game_id": game_id
                }),
                "log_link": "{}/user/{}/match/{}/error_log".format(
                    config.API_URL, user["user_id"], game_id),
            },
            config.GAME_ERROR_MESSAGES,
            config.C_BOT_DISABLED)

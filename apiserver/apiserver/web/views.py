import io

import flask
import sqlalchemy
import google.cloud.storage as gcloud_storage

from .. import config, model, util
# TODO: get rid of response_failure
from .. import optional_login, requires_login, response_failure, response_success


web_api = flask.Blueprint("web_api", __name__)


def user_mismatch_error(message="Cannot perform action for other user."):
    raise util.APIError(400, message=message)


def get_offset_limit(*, default_limit=10, max_limit=100):
    offset = int(flask.request.values.get("offset", 0))
    offset = max(offset, 0)
    limit = int(flask.request.values.get("limit", default_limit))
    limit = min(max(limit, 0), max_limit)

    return offset, limit

######################
# USER API ENDPOINTS #
######################


@web_api.route("/user")
def list_users():
    result = []
    offset, limit = get_offset_limit()

    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().offset(offset).limit(limit))

        for row in query.fetchall():
            result.append({
                "user_id": row["userID"],
                "username": row["username"],
                "level": row["level"],
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
            })

    return flask.jsonify(result)


@web_api.route("/user", methods=["POST"])
def create_user():
    # TODO: investigate how this works with OAuth/Github (this endpoint redirects to Github?)
    pass


@web_api.route("/user/<int:user_id>", methods=["GET"])
def get_user(user_id):
    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().where(
            model.users.c.userID == user_id
        )).fetchall()

        if len(query) != 1:
            return response_failure("Could not find user.")

        row = query[0]
        return flask.jsonify({
            "user_id": row["userID"],
            "username": row["username"],
            "level": row["level"],
            # In the future, these would be the stats of their best bot
            # Right now, though, each user has at most 1 bot
            "rank": row["rank"],
            "points": row["mu"],
            "num_submissions": row["numSubmissions"],
            "num_games": row["numGames"],
        })


@web_api.route("/user/<int:intended_user_id>", methods=["PUT"])
@requires_login
def update_user(user_id, intended_user_id):
    pass


@web_api.route("/user/<int:intended_user_id>", methods=["DELETE"])
@requires_login
def delete_user(user_id, intended_user_id):
    pass


# ---------------------- #
# USER BOT API ENDPOINTS #
# ---------------------- #

# Currently, there is no notion of a user having multiple distinct bots.
# However, in the API, we pretend this is the case as much as possible, since
# we may wish to support this eventually.


@web_api.route("/user/<int:user_id>/bot", methods=["GET"])
def list_user_bots(user_id):
    # TODO: parameter to get only IDs

    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().where(
            model.users.c.userID == user_id
        )).fetchall()

        if len(query) != 1:
            raise util.APIError(404, message="User not found.")

        row = query[0]

        if row["numSubmissions"] == 0:
            return flask.jsonify([])

        return flask.jsonify([
            {
                "bot_id": 0,
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "language": row["language"],
            }
        ])


@web_api.route("/user/<int:user_id>/bot/<int:bot_id>", methods=["GET"])
def get_user_bot(user_id, bot_id):
    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().where(
            model.users.c.userID == user_id
        )).fetchall()

        if len(query) != 1:
            raise util.APIError(404, message="User not found.")

        if bot_id != 0:
            raise util.APIError(404, message="Bot not found.")

        row = query[0]

        if row["numSubmissions"] == 0:
            return flask.jsonify([])

        return flask.jsonify([
            {
                "bot_id": 0,
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "language": row["language"],
            }
        ])


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["PUT"])
@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["POST"])
@requires_login
def store_user_bot(user_id, intended_user, bot_id):
    """Store an uploaded bot in object storage."""
    if not config.COMPETITION_OPEN:
        return response_failure("Sorry, but bot submissions are closed.")

    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot upload bot for another user.")

    conn = model.engine.connect()
    user = conn.execute(model.users.select(model.users.c.userID == user_id)) \
        .first()

    # Check if the user already has a bot compiling
    if user["compileStatus"] != 0:
        raise util.APIError(400, message="Cannot upload new bot until "
                                         "previous one is compiled.")

    if "botFile" not in flask.request.files:
        raise util.APIError(400, message="Bot file not provided (must "
                                         "provide as botFile).")

    # Save to GCloud
    uploaded_file = flask.request.files["botFile"]
    blob = gcloud_storage.Blob(str(user_id), model.get_compilation_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)

    # Flag the user as compiling
    update = model.users.update() \
        .where(model.users.c.userID == user_id) \
        .values(compileStatus=1)
    conn.execute(update)

    # TODO: Email the user

    # TODO: Spin up more workers?

    return response_success()


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["DELETE"])
@requires_login
def delete_user_bot(intended_user, bot_id, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot delete bot for another user.")

    # TODO: implement


# ------------------------ #
# USER MATCH API ENDPOINTS #
# ------------------------ #
@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = get_offset_limit()
    result = []

    with model.engine.connect() as conn:
        query = conn.execute(sqlalchemy.sql.select([
            model.games.c.gameID,
            model.games.c.replayName,
            model.games.c.mapWidth,
            model.games.c.mapHeight,
            model.games.c.timestamp,
        ]).select_from(model.games.join(
            model.gameusers,
            (model.games.c.gameID == model.gameusers.c.gameID) &
            (model.gameusers.c.userID == intended_user),
        )).offset(offset).limit(limit))

        for match in query.fetchall():
            result.append({
                "game_id": match["gameID"],
                "map_width": match["mapWidth"],
                "map_height": match["mapHeight"],
                "replay": match["replayName"],
                "timestamp": match["timestamp"],
            })

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>", methods=["GET"])
@optional_login
def get_user_match(intended_user, match_id, *, user_id):
    with model.engine.connect() as conn:
        query = conn.execute(sqlalchemy.sql.select([
            model.gameusers.c.userID,
            model.gameusers.c.rank,
            model.gameusers.c.playerIndex,
            model.gameusers.c.didTimeout,
            model.gameusers.c.errorLogName,
        ]).where(
            model.gameusers.c.gameID == match_id
        ))

        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replayName,
            model.games.c.mapWidth,
            model.games.c.mapHeight,
            model.games.c.timestamp,
        ]).where(
            model.games.c.gameID == match_id
        )).first()

        result = {
            "map_width": match["mapWidth"],
            "map_height": match["mapHeight"],
            "replay": match["replayName"],
            "timestamp": match["timestamp"],
            "players": {}
        }
        for row in query.fetchall():
            result["game_id"] = match_id
            result["players"][row["userID"]] = {
                "rank": row["rank"],
                "player_index": row["playerIndex"],
                "timed_out": bool(row["didTimeout"]),
            }

            if (user_id is not None and intended_user == user_id and
                    row["userID"] == user_id):
                result["players"][row["userID"]]["error_log"] = row["errorLogName"]

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/replay",
               methods=["GET"])
def get_match_replay(intended_user, match_id):
    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replayName,
        ]).where(
            model.games.c.gameID == match_id
        )).first()

        blob = gcloud_storage.Blob(match["replayName"],
                                   model.get_replay_bucket(),
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(buffer, mimetype="application/x-halite-2-replay",
                               as_attachment=True,
                               attachment_filename=str(match_id)+".hlt")


##############################
# ORGANIZATION API ENDPOINTS #
##############################
@web_api.route("/organization")
def list_organizations():
    pass


@web_api.route("/organization/<name>", methods=["GET"])
def get_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["POST"])
def create_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["PUT"])
def update_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["DELETE"])
def delete_organization(name):
    pass


#############################
# LEADERBOARD API ENDPOINTS #
#############################
@web_api.route("/leaderboard")
def leaderboard():
    # TODO: filtering and ordering options, asc/desc
    offset, limit = get_offset_limit()
    result = []

    with model.engine.connect() as conn:
        players = conn.execute(sqlalchemy.sql.select([
            model.users.c.userID,
            model.users.c.username,
            model.users.c.level,
            model.users.c.organization,
            model.users.c.language,
            model.users.c.mu,
            model.users.c.rank,
        ]).order_by(model.users.c.rank.asc()).offset(offset).limit(limit))

        for player in players.fetchall():
            result.append({
                "user_id": player["userID"],
                "username": player["username"],
                "level": player["level"],
                "organization": player["organization"],
                "language": player["language"],
                "points": player["mu"],
                "rank": player["rank"],
            })

    return flask.jsonify(result)

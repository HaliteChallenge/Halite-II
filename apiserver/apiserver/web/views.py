import io
import operator

import arrow
import flask
import sqlalchemy
import sqlalchemy.orm.exc as sqlalchemy_exc
import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions

from .. import config, model, util
from .. import optional_login, requires_login, response_success


web_api = flask.Blueprint("web_api", __name__)


def user_mismatch_error(message="Cannot perform action for other user."):
    raise util.APIError(400, message=message)


def get_offset_limit(*, default_limit=10, max_limit=100):
    offset = int(flask.request.values.get("offset", 0))
    offset = max(offset, 0)
    limit = int(flask.request.values.get("limit", default_limit))
    limit = min(max(limit, 0), max_limit)

    return offset, limit


def get_sort_filter(fields):
    """
    Parse flask.request to create clauses for SQLAlchemy's order_by and where.

    :param fields: A dictionary of field names to SQLAlchemy table columns
    listing what fields can be sorted/ordered on.
    :return: A 2-tuple of (where_clause, order_clause). order_clause is an
    ordered list of columns.
    """
    where_clause = True
    order_clause = []

    for filter_param in flask.request.args.getlist("filter"):
        field, cmp, value = filter_param.split(",")

        if field not in fields:
            raise util.APIError(
                400, message="Cannot filter on field {}".format(field))

        column = fields[field]
        conversion = None
        if isinstance(column.type, sqlalchemy.types.Integer):
            conversion = int
        elif isinstance(column.type, sqlalchemy.types.DateTime):
            conversion = lambda x: arrow.get(x).datetime
        elif isinstance(column.type, sqlalchemy.types.Float):
            conversion = float
        elif isinstance(column.type, sqlalchemy.types.String):
            conversion = lambda x: x
        else:
            raise RuntimeError("Filtering on column is not supported yet: " + repr(column))

        value = conversion(value)
        operation = {
            "=": operator.eq,
            "<": operator.lt,
            "<=": operator.le,
            ">": operator.gt,
            ">=": operator.ge,
            "!=": operator.ne,
        }.get(cmp, None)

        if operation is None:
            raise util.APIError(
                400, message="Cannot compare column by '{}'".format(cmp))

        clause = operation(column, value)
        if where_clause is True:
            where_clause = clause
        else:
            where_clause &= clause


    for order_param in flask.request.args.getlist("order_by"):
        direction = "asc"
        if "," in order_param:
            direction, field = order_param.split(",")
        else:
            field = order_param

        if field not in fields:
            raise util.APIError(
                400, message="Cannot order on field {}".format(field))

        column = fields[field]
        if direction == "asc":
            column = column.asc()
        elif direction == "desc":
            column = column.desc()
        else:
            raise util.APIError(
                400, message="Cannot order column by '{}'".format(direction))

        order_clause.append(column)

    return where_clause, order_clause

######################
# USER API ENDPOINTS #
######################


@web_api.route("/user")
def list_users():
    result = []
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "user_id": model.users.c.userID,
        "username": model.users.c.username,
        "level": model.users.c.level,
        "rank": model.users.c.rank,
        "num_submissions": model.users.c.numSubmissions,
        "num_games": model.users.c.numGames,
        "organization_id": model.users.c.organizationID,
    })

    with model.engine.connect() as conn:
        query = conn.execute(
            model.users.select()
                .where(where_clause).order_by(*order_clause)
                .offset(offset).limit(limit)
        )

        for row in query.fetchall():
            result.append({
                "user_id": row["userID"],
                "username": row["username"],
                "level": row["level"],
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "organization_id": row["organization_id"],
            })

    return flask.jsonify(result)


@web_api.route("/user", methods=["POST"])
def create_user():
    # TODO: investigate how this works with OAuth/Github (this endpoint redirects to Github?)
    pass


@web_api.route("/user/<int:user_id>", methods=["GET"])
def get_user(user_id):
    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.users.c.userID,
            model.users.c.username,
            model.users.c.level,
            model.users.c.organizationID,
            model.users.c.rank,
            model.users.c.mu,
            model.users.c.sigma,
            model.users.c.numSubmissions,
            model.users.c.numGames,
            model.organizations.c.organizationName,
        ]).where(
            model.users.c.userID == user_id
        ).select_from(
            model.users.join(
                model.organizations,
                model.users.c.organizationID == model.organizations.c.organizationID)
        )

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        return flask.jsonify({
            "user_id": row["userID"],
            "username": row["username"],
            "level": row["level"],
            "organization_id": row["organizationID"],
            "organization": row["organizationName"],
            # In the future, these would be the stats of their best bot
            # Right now, though, each user has at most 1 bot
            "rank": row["rank"],
            "points": row["mu"] - 3 * row["sigma"],
            "num_submissions": row["numSubmissions"],
            "num_games": row["numGames"],
        })


@web_api.route("/user/<int:intended_user_id>", methods=["PUT"])
@requires_login
def update_user(intended_user_id, *, user_id):
    pass


@web_api.route("/user/<int:intended_user_id>", methods=["DELETE"])
@requires_login
def delete_user(intended_user_id, *, user_id):
    if user_id != intended_user_id:
        raise user_mismatch_error()

    with model.engine.connect() as conn:
        conn.execute(model.users.delete().where(
            model.users.c.userID == user_id))


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

# TODO: POST to just /bot to create a new bot
@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["PUT"])
@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["POST"])
@requires_login
def store_user_bot(user_id, intended_user, bot_id):
    """Store an uploaded bot in object storage."""
    if not config.COMPETITION_OPEN:
        raise util.APIError(
            400, message="Sorry, but bot submissions are closed."
        )

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

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.userID == user_id
        ).values(
            isRunning=0,
        ))

        for bucket in [model.get_bot_bucket(), model.get_compilation_bucket()]:
            try:
                blob = gcloud_storage.Blob(str(user_id), bucket)
                blob.delete()
            except gcloud_exceptions.NotFound:
                pass

        return response_success()


# ------------------------ #
# USER MATCH API ENDPOINTS #
# ------------------------ #
@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "game_id": model.games.c.gameID,
        "timestamp": model.games.c.timestamp,
    })
    result = []

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.games.c.gameID,
            model.games.c.replayName,
            model.games.c.mapWidth,
            model.games.c.mapHeight,
            model.games.c.timestamp,
        ]).select_from(model.games.join(
            model.gameusers,
            (model.games.c.gameID == model.gameusers.c.gameID) &
            (model.gameusers.c.userID == intended_user),
        )).where(where_clause).order_by(*order_clause).offset(offset).limit(limit)
        matches = conn.execute(query)

        for match in matches.fetchall():
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


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/error_log",
               methods=["GET"])
@requires_login
def get_match_error_log(intended_user, match_id, *, user_id):
    """
    Serve the error log for a user's bot in a particular match.

    Only allows a logged-in user to download their own error log.
    """

    if intended_user != user_id:
        raise util.APIError(
            404, message="Cannot find error log. You must be signed in, "
                         "and you can only request your error log. "
        )

    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.gameusers.c.errorLogName,
        ]).where(
            (model.gameusers.c.gameID == match_id) &
            (model.gameusers.c.userID == user_id)
        )).first()

        if match is None:
            raise util.APIError(
                404, message="Game does not exist."
            )

        if match["errorLogName"] is None:
            raise util.APIError(
                404, message="No error log for this player in this game."
            )

        blob = gcloud_storage.Blob(match["errorLogName"],
                                   model.get_error_log_bucket(),
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(
            buffer, mimetype="text/plain", as_attachment=True,
            attachment_filename="match_{}_user_{}_errors.log".format(match_id, user_id))


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
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "user_id": model.users.c.userID,
        "username": model.users.c.username,
        "level": model.users.c.level,
        "organization_id": model.users.c.organizationID,
        "language": model.users.c.language,
        "points": model.users.c.mu,
        "rank": model.users.c.rank,
    })
    if not order_clause:
        order_clause = [model.users.c.rank.asc()]
    result = []

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.users.c.userID,
            model.users.c.username,
            model.users.c.level,
            model.users.c.organizationID,
            model.organizations.c.organizationName,
            model.users.c.language,
            model.users.c.mu,
            model.users.c.rank,
        ]).select_from(
            model.users.join(
                model.organizations,
                model.users.c.organizationID == model.organizations.c.organizationID)
        ).where(where_clause).order_by(*order_clause).offset(offset).limit(limit)
        players = conn.execute(query)

        for player in players.fetchall():
            result.append({
                "user_id": player["userID"],
                "username": player["username"],
                "level": player["level"],
                "organization_id": player["organizationID"],
                "organization": player["organizationName"],
                "language": player["language"],
                "points": player["mu"],
                "rank": player["rank"],
            })

    return flask.jsonify(result)

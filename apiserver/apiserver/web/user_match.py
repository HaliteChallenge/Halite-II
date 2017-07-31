"""
User match API endpoints - list user matches and get replays/error logs
"""
import io

import flask
import sqlalchemy
import google.cloud.storage as gcloud_storage

from .. import model, util
from ..util import cross_origin

from .util import get_offset_limit, get_sort_filter, requires_login
from .blueprint import web_api


@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
@cross_origin(methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = get_offset_limit()
    where_clause, order_clause, manual_sort = get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
        # TODO: filter by participants
    }, ["timed_out"])
    result = []

    participant_clause = sqlalchemy.true()
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.games.c.id,
            model.games.c.replay_name,
            model.games.c.map_width,
            model.games.c.map_height,
            model.games.c.time_played,
        ]).select_from(model.games.join(
            model.game_participants,
            (model.games.c.id == model.game_participants.c.game_id) &
            (model.game_participants.c.user_id == intended_user) &
            participant_clause,
            )).where(where_clause).order_by(*order_clause).offset(offset).limit(limit).reduce_columns()
        matches = conn.execute(query)

        for match in matches.fetchall():
            participants = conn.execute(model.game_participants.select(
                model.game_participants.c.game_id == match["id"]
            ))

            match = {
                "game_id": match["id"],
                "map_width": match["map_width"],
                "map_height": match["map_height"],
                "replay": match["replay_name"],
                "time_played": match["time_played"],
                "players": {},
            }

            for participant in participants:
                match["players"][participant["user_id"]] = {
                    "bot_id": participant["bot_id"],
                    "version_number": participant["version_number"],
                    "player_index": participant["player_index"],
                    "rank": participant["rank"],
                    "timed_out": bool(participant["timed_out"]),
                }

            result.append(match)

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>", methods=["GET"])
@cross_origin(methods=["GET"])
def get_user_match(intended_user, match_id):
    with model.engine.connect() as conn:
        query = conn.execute(sqlalchemy.sql.select([
            model.game_participants.c.user_id,
            model.game_participants.c.bot_id,
            model.game_participants.c.rank,
            model.game_participants.c.version_number,
            model.game_participants.c.player_index,
            model.game_participants.c.timed_out,
        ]).where(
            model.game_participants.c.game_id == match_id
        ))

        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replay_name,
            model.games.c.map_width,
            model.games.c.map_height,
            model.games.c.time_played,
        ]).where(
            model.games.c.id == match_id
        )).first()

        result = {
            "map_width": match["map_width"],
            "map_height": match["map_height"],
            "replay": match["replay_name"],
            "time_played": match["time_played"],
            "players": {}
        }
        for row in query.fetchall():
            result["game_id"] = match_id
            result["players"][row["user_id"]] = {
                "bot_id": row["bot_id"],
                "version_number": row["version_number"],
                "player_index": row["player_index"],
                "rank": row["rank"],
                "timed_out": bool(row["timed_out"]),
            }

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/replay",
               methods=["GET"])
@cross_origin(methods=["GET"])
def get_match_replay(intended_user, match_id):
    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replay_name,
            model.games.c.replay_bucket,
        ]).where(
            model.games.c.id == match_id
        )).first()

        bucket = model.get_replay_bucket(match["replay_bucket"])
        blob = gcloud_storage.Blob(match["replay_name"], bucket,
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        response = flask.make_response(flask.send_file(
            buffer,
            mimetype="application/x-halite-2-replay",
            as_attachment=True,
            attachment_filename=str(match_id)+".hlt"))

        response.headers["Content-Length"] = str(buffer.getbuffer().nbytes)

        return response


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
            model.game_participants.c.log_name,
        ]).where(
            (model.game_participants.c.game_id == match_id) &
            (model.game_participants.c.user_id == user_id)
        )).first()

        if match is None:
            raise util.APIError(
                404, message="Game does not exist."
            )

        if match["log_name"] is None:
            raise util.APIError(
                404, message="No error log for this player in this game."
            )

        blob = gcloud_storage.Blob(match["log_name"],
                                   model.get_error_log_bucket(),
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(
            buffer, mimetype="text/plain", as_attachment=True,
            attachment_filename="match_{}_user_{}_errors.log".format(match_id, user_id))
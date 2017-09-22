"""
User match API endpoints - list user matches and get replays/error logs
"""
import io

import flask
import sqlalchemy
import google.cloud.storage as gcloud_storage

from .. import model, util

from . import match as match_api
from . import util as api_util
from .blueprint import web_api


@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
@util.cross_origin(methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
        # TODO: filter by participants
    }, ["timed_out"])

    participant_clause = model.game_participants.c.user_id == intended_user
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

    result = match_api.list_matches_helper(
        offset, limit, participant_clause, where_clause, order_clause)

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>", methods=["GET"])
@util.cross_origin(methods=["GET"])
def get_user_match(intended_user, match_id):
    result = match_api.get_match_helper(match_id)
    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/replay",
               methods=["GET"])
@util.cross_origin(methods=["GET"])
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
@api_util.requires_login(accept_key=True)
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
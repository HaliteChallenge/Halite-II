"""
User challenge API endpoints - list user's challenges & issue new ones
"""
import datetime

import flask
import sqlalchemy

from .. import config, model, util

from . import match as match_api
from . import challenge as challenge_api
from . import util as api_util
from .blueprint import web_api


@web_api.route("/user/<int:intended_user>/challenge", methods=["GET"])
@util.cross_origin(methods=["GET", "POST"])
def list_user_challenges(intended_user):
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "issuer": model.challenges.c.issuer,
        "created": model.challenges.c.created,
        "finished": model.challenges.c.finished,
        "num_games": model.challenges.c.num_games,
        "winner": model.challenges.c.winner,
        "status": model.challenges.c.status,
        "id": model.challenges.c.id,
    }, ["finished"])

    participant_clause = model.challenge_participants.c.user_id == intended_user
    for (field, _, _) in manual_sort:
        if field == "finished":
            where_clause &= model.challenges.c.status == "finished"

    result = challenge_api.list_challenges_helper(offset, limit,
                                                  participant_clause,
                                                  where_clause, order_clause)
    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/challenge/<int:challenge_id>", methods=["GET"])
@util.cross_origin(methods=["GET"])
def get_user_challenge(intended_user, challenge_id):
    result = challenge_api.get_challenge_helper(challenge_id)
    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/challenge/<int:challenge_id>/match", methods=["GET"])
@util.cross_origin(methods=["GET", "POST"])
def list_user_challenge_matches(intended_user, challenge_id):
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
    }, ["timed_out"])

    participant_clause = model.game_participants.c.user_id == intended_user
    where_clause &= model.games.c.challenge_id == challenge_id
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

    result = match_api.list_matches_helper(
        offset, limit, participant_clause, where_clause, order_clause)

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/challenge", methods=["POST"])
@util.cross_origin(methods=["GET", "POST"])
@api_util.requires_login(accept_key=False)
@api_util.requires_competition_open
def create_challenge(intended_user, *, user_id):
    if user_id != intended_user:
        raise api_util.user_mismatch_error()

    challenge_body = flask.request.get_json()
    if "opponents" not in challenge_body:
        raise util.APIError(400, message="Must provide array of opponent IDs.")

    opponents = challenge_body["opponents"]
    if user_id in opponents:
        raise util.APIError(400, message="You can't challenge yourself.")
    if len(opponents) not in (1, 3):
        raise util.APIError(400, message="Must provide 1 or 3 opponents.")

    if config.COMPETITION_FINALS_PAIRING:
        raise util.APIError(400, message="Sorry, challenges are closed for the competition finals.")

    with model.engine.connect() as conn:
        sqlfunc = sqlalchemy.sql.func

        opponents_exist = [row["id"] for row in conn.execute(
            sqlalchemy.sql.select([
                model.users.c.id,
            ]).select_from(
                model.users
            ).where(
                model.users.c.id.in_(opponents)
            )
        ).fetchall()]

        if len(opponents_exist) != len(opponents):
            raise util.APIError(400, message="Opponents {} do not exist.".format(
                ", ".join(map(str, set(opponents) - set(opponents_exist)))
            ))

        num_challenges = conn.execute(
            sqlalchemy.sql.select([
                sqlfunc.count(),
            ]).select_from(
                model.challenges
            ).where(
                (model.challenges.c.issuer == user_id) &
                (model.challenges.c.created >= sqlfunc.date_sub(
                    sqlfunc.now(), sqlalchemy.sql.text("interval 1 day")))
            )
        ).first()[0]

        if num_challenges >= 3:
            raise util.APIError(
                400,
                message="Can't issue more than 3 challenges in a 24 hour period."
            )

        with conn.begin() as transaction:
            challenge_id = conn.execute(model.challenges.insert().values(
                issuer=user_id,
                num_games=0,
            )).inserted_primary_key[0]

            opponents.append(user_id)
            for participant in opponents:
                conn.execute(model.challenge_participants.insert().values(
                    challenge_id=challenge_id,
                    user_id=participant,
                    points=0,
                    ships_produced=0,
                    attacks_made=0,
                ))

        return util.response_success({
            "challenge_id": challenge_id,
        }, status_code=201)

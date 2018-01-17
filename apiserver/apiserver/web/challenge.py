"""
User challenge API endpoints - list user's challenges & issue new ones
"""
import datetime

import flask
import sqlalchemy

from .. import model, util

from . import match as match_api
from . import util as api_util
from .blueprint import web_api


def make_challenge_record(challenge, participants):
    result = {
        "challenge_id": challenge["id"],
        "time_created": challenge["created"],
        "time_finished": challenge["finished"],
        "num_games": challenge["num_games"],
        "issuer": challenge["issuer"],
        "winner": challenge["winner"],
        "finished": bool(challenge["finished"]),
        "players": {},
    }

    for participant in participants:
        result["players"][participant["user_id"]] = {
            "username": participant["username"],
            "points": participant["points"],
            "ships_produced": participant["ships_produced"],
            "attacks_made": participant["attacks_made"],
            "is_issuer": participant["user_id"] == result["issuer"],
        }

    return result


def get_challenge_helper(challenge_id):
    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.challenges.c.id,
            model.challenges.c.created,
            model.challenges.c.finished,
            model.challenges.c.num_games,
            model.challenges.c.issuer,
            model.challenges.c.winner,
        ]).select_from(model.challenges).where(
            model.challenges.c.id == challenge_id
        ).reduce_columns()

        challenge = conn.execute(query).first()
        if not challenge:
            raise util.APIError(
                404,
                message="Challenge {} not found.".format(challenge_id))

        participants = conn.execute(
            model.challenge_participants.join(
                model.users,
                model.challenge_participants.c.user_id == model.users.c.id
            ).select(
                model.challenge_participants.c.challenge_id == challenge["id"]
            )
        )
        return make_challenge_record(challenge, participants)


def list_challenges_helper(offset, limit, participant_clause,
                           where_clause, order_clause):
    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.challenges.c.id,
            model.challenges.c.created,
            model.challenges.c.finished,
            model.challenges.c.num_games,
            model.challenges.c.issuer,
            model.challenges.c.winner,
        ]).select_from(model.challenges).where(
            where_clause &
            sqlalchemy.sql.exists(model.challenge_participants.select(
                participant_clause &
                (model.challenges.c.id == model.challenge_participants.c.challenge_id)
            ).correlate(model.challenges))
        ).order_by(*order_clause).offset(offset).limit(limit).reduce_columns()

        challenges = conn.execute(query)
        result = []
        for challenge in challenges.fetchall():
            participants = conn.execute(sqlalchemy.sql.select([
                model.challenge_participants.c.user_id,
                model.challenge_participants.c.points,
                model.challenge_participants.c.ships_produced,
                model.challenge_participants.c.attacks_made,
                model.users.c.username,
            ]).select_from(model.challenge_participants.join(
                model.users,
                model.challenge_participants.c.user_id == model.users.c.id
            )).where(
                model.challenge_participants.c.challenge_id == challenge["id"]
            )).fetchall()

            result.append(make_challenge_record(challenge, participants))

        return result


@web_api.route("/challenge", methods=["GET"])
@util.cross_origin(methods=["GET"])
def list_challenges():
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "issuer": model.challenges.c.issuer,
        "created": model.challenges.c.created,
        "finished": model.challenges.c.finished,
        "num_games": model.challenges.c.num_games,
        "winner": model.challenges.c.winner,
        "status": model.challenges.c.status,
        "id": model.challenges.c.id,
    }, ["finished", "participant"])

    participant_clause = sqlalchemy.true()
    for (field, op, val) in manual_sort:
        if field == "finished":
            where_clause &= model.challenges.c.status == "finished"
        elif field == "participant":
            participant_clause &= op(model.challenge_participants.c.user_id, val)

    result = list_challenges_helper(offset, limit,
                                    participant_clause,
                                    where_clause, order_clause)
    return flask.jsonify(result)


@web_api.route("/challenge/<int:challenge_id>", methods=["GET"])
@util.cross_origin(methods=["GET"])
def get_challenge(challenge_id):
    result = get_challenge_helper(challenge_id)
    return flask.jsonify(result)


@web_api.route("/challenge/<int:challenge_id>/match", methods=["GET"])
@util.cross_origin(methods=["GET"])
def list_challenge_matches(challenge_id):
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
    }, ["timed_out"])

    participant_clause = sqlalchemy.true()
    where_clause &= model.games.c.challenge_id == challenge_id
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

    result = match_api.list_matches_helper(
        offset, limit, participant_clause, where_clause, order_clause)

    return flask.jsonify(result)

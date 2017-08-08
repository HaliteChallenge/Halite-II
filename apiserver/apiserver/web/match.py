"""
Match API endpoints - list matches and get replays/error logs
"""

import flask
import sqlalchemy

from .. import model

from .blueprint import web_api
from . import util as api_util


def get_match_helper(match_id):
    """
    Get a particular match by its ID.

    :param match_id: The ID of the match.
    :return: A dictionary with the game information.
    """

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

    return result


def list_matches_helper(offset, limit, participant_clause,
                        where_clause, order_clause):
    """
    Generate a list of matches by certain criteria.

    :param int offset: How
    :param int limit: How many results to return.
    :param participant_clause: An SQLAlchemy clause to filter the matches,
    based on the participants in the match.
    :param where_clause: An SQLAlchemy clause to filter the matches.
    :param list order_clause: A list of SQLAlchemy conditions to sort the
    results on.
    :return: A list of game data dictionaries.
    """
    result = []

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
            participant_clause,
            )
        ).where(
            where_clause
        ).order_by(
            *order_clause
        ).group_by(
            # Get rid of duplicates when not filtering by a
            # particular participant
            model.games.c.id
        ).offset(offset).limit(limit).reduce_columns()
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

    return result


@web_api.route("/match")
def list_matches():
    offset, limit = api_util.get_offset_limit()
    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
    }, ["timed_out"])

    participant_clause = sqlalchemy.true()
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

    result = list_matches_helper(
        offset, limit, participant_clause, where_clause, order_clause)

    return flask.jsonify(result)


@web_api.route("/match/<int:match_id>")
def get_match(match_id):
    return flask.jsonify(get_match_helper(match_id))

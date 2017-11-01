"""
Leaderboard API endpoints - get/sort/filter the leaderboard
"""
import operator

import flask
import sqlalchemy

from .. import config, model, util

from . import util as api_util
from .blueprint import web_api

_COUNT_KEY = 'count'
_LEADERBOARD_ALIAS = 'full_leaderboard'


def _count_leaderboard_query(where_clause):
    """
    Considering the desired clause, return the number of distinct users in leaderboard
    :param where_clause: Clause to filter by
    :return: Number of distinct users
    """
    full_leaderboard = model.ranked_bots_users.select().where(where_clause).reduce_columns().alias(_LEADERBOARD_ALIAS)
    return sqlalchemy.sql.select([
        sqlalchemy.sql.func.count(sqlalchemy.distinct(full_leaderboard.c.user_id))
    ]).select_from(full_leaderboard)


@web_api.route("/leaderboard")
@util.cross_origin(methods=["GET"])
def leaderboard():
    result = []
    offset, limit = api_util.get_offset_limit(default_limit=250,
                                              max_limit=5000)

    where_clause, order_clause, manual_sort = api_util.get_sort_filter({
        "user_id": model.ranked_bots_users.c.user_id,
        "username": model.ranked_bots_users.c.username,
        "level": model.ranked_bots_users.c.player_level,
        "organization_id": model.ranked_bots_users.c.organization_id,
        "organization_name": model.ranked_bots_users.c.organization_name,
        "country_code": model.ranked_bots_users.c.country_code,
        "country_subdivision_code": model.ranked_bots_users.c.country_subdivision_code,
        "version_number": model.ranked_bots_users.c.num_submissions,
        "num_games": model.ranked_bots_users.c.num_games,
        "rank": model.ranked_bots_users.c.rank,
        "language": model.ranked_bots_users.c.language,
    }, ["tier"])

    if not order_clause:
        order_clause = [model.ranked_bots_users.c.rank]

    with model.engine.connect() as conn:
        if _COUNT_KEY in flask.request.args:
            return str(api_util.get_value(conn.execute(_count_leaderboard_query(where_clause))))

        total_users = api_util.get_value(conn.execute(model.total_ranked_users))

        tier_filter = None
        tier_thresholds = util.tier_thresholds(total_users)
        for (field, op, val) in manual_sort:
            if field == "tier":
                column = model.ranked_bots_users.c.rank
                tier = val
                if val in tier_thresholds:
                    val = tier_thresholds[val]
                else:
                    raise util.APIError(
                        400,
                        message="Tier " + str(val) + " is not recognized.")

                next_tier_dict = {
                    config.TIER_0_NAME: None,
                    config.TIER_1_NAME: tier_thresholds[config.TIER_0_NAME],
                    config.TIER_2_NAME: tier_thresholds[config.TIER_1_NAME],
                    config.TIER_3_NAME: tier_thresholds[config.TIER_2_NAME],
                    config.TIER_4_NAME: tier_thresholds[config.TIER_3_NAME],
                }
                # Negate the filter, since tier and rank are sorted
                # opposite of each other
                if op in (operator.gt, operator.lt, operator.ge, operator.le):
                    if op in (operator.gt, operator.le):
                        val = next_tier_dict.get(tier, val)
                    clause = {
                        operator.gt: operator.le,
                        operator.lt: operator.gt,
                        operator.ge: operator.le,
                        operator.le: operator.gt,
                    }[op](column, val)
                elif op is operator.eq or op is operator.ne:
                    clause = column <= val
                    next_tier = next_tier_dict.get(tier)
                    if next_tier is not None:
                        clause &= column > next_tier

                    if op is operator.ne:
                        clause = ~clause

                if tier_filter is None:
                    tier_filter = clause
                else:
                    tier_filter |= clause

        if tier_filter is not None:
            where_clause &= tier_filter

        query = conn.execute(
            model.ranked_bots_users.select()
                .where(where_clause).order_by(*order_clause)
                .offset(offset).limit(limit).reduce_columns())

        for row in query.fetchall():
            user = {
                "user_id": row["user_id"],
                "username": row["username"],
                "level": row["player_level"],
                "organization_id": row["organization_id"],
                "organization": row["organization_name"],
                "version_number": int(row["num_submissions"]),
                "num_games": int(row["num_games"]),
                "score": float(row["score"]),
                "language": row["language"],
                "country": row["country_code"],
                "rank": int(row["rank"]) if row["rank"] is not None else None,
                "update_time": row["update_time"],
                "mu": row["mu"],
                "sigma": row["sigma"],
            }

            if total_users and row["rank"] is not None:
                user["tier"] = util.tier(row["rank"], total_users)
            else:
                user["tier"] = None

            result.append(user)

    return flask.jsonify(result)


@web_api.route("/leagues")
@util.cross_origin(methods=["GET"])
def leagues():
    result = []
    with model.engine.connect() as conn:
        query = conn.execute(
            model.leagues.select())

        for row in query.fetchall():
            league = {
                "id": row["id"],
                "category": row["category"],
                "name": row["name"],
                "description": row["description"],
                "query": row["query"],
            }

            result.append(league)

    return flask.jsonify(result)
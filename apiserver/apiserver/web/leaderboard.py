"""
Leaderboard API endpoints - get/sort/filter the leaderboard
"""
import operator

import flask
import sqlalchemy

from .. import model, util

from . import util as api_util
from .blueprint import web_api


@web_api.route("/leaderboard")
@util.cross_origin(methods=["GET"])
def leaderboard():
    result = []
    offset, limit = api_util.get_offset_limit()

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
        total_users = conn.execute(model.total_ranked_users).first()[0]

        tier_filter = sqlalchemy.false()
        tier_thresholds = util.tier_thresholds(total_users)
        for (field, op, val) in manual_sort:
            if field == "tier":
                column = model.ranked_bots_users.c.rank
                if val in tier_thresholds:
                    val = tier_thresholds[val]
                else:
                    raise util.APIError(
                        400,
                        message="Tier " + str(val) + " is not recognized.")
                clause = op(column, val)
                # Negate the filter, since tier and rank are sorted
                # opposite of each other
                if op in (operator.gt, operator.lt, operator.ge, operator.le):
                    clause = ~clause
                tier_filter |= clause
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
                "rank": int(row["rank"]) if row["rank"] is not None else None,
            }

            if total_users and row["rank"] is not None:
                user["tier"] = util.tier(row["rank"], total_users)
            else:
                user["tier"] = None

            result.append(user)

    return flask.jsonify(result)

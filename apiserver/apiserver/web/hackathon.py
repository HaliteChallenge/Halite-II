"""
Hackathon API endpoints - create/update/delete/list hackathons+leaderboards
"""

import uuid

import arrow
import flask
import sqlalchemy

from .. import model, util

from . import util as api_util
from .blueprint import web_api


def can_view_hackathon(user_id, hackathon_id, conn):
    """
    Validate whether a given user is allowed to view a given hackathon.
    :param user_id:
    :param hackathon_id:
    :param conn:
    :return: True if the user can view the hackathon; False otherwise
    """
    is_user_joined = conn.execute(model.hackathon_participants.select(
        (model.hackathon_participants.c.user_id == user_id) &
        (model.hackathon_participants.c.hackathon_id == hackathon_id)
    )).first()

    is_admin = api_util.is_user_admin(user_id, conn=conn)

    return is_user_joined or is_admin


hackathon_query = sqlalchemy.sql.select([
    model.hackathons.c.id,
    model.hackathons.c.title,
    model.hackathons.c.description,
    model.hackathons.c.start_date,
    model.hackathons.c.end_date,
    model.hackathons.c.organization_id,
    model.organizations.c.organization_name,
    model.hackathons.c.location,
    model.hackathons.c.thumbnail,
]).select_from(
    model.hackathons.join(
        model.organizations,
        model.hackathons.c.organization_id == model.organizations.c.id,
        isouter=True,
        )
)


@web_api.route("/hackathon", methods=["GET"])
@api_util.requires_login(accept_key=True, admin=True)
def list_hackathons(*, user_id):
    result = []
    offset, limit = api_util.get_offset_limit()

    with model.engine.connect() as conn:
        hackathons = conn.execute(
            hackathon_query.offset(offset).limit(limit)).fetchall()

        for hackathon in hackathons:
            record = {
                "hackathon_id": hackathon["id"],
                "title": hackathon["title"],
                "description": hackathon["description"],
                "status": api_util.hackathon_status(hackathon["start_date"],
                                                    hackathon["end_date"]),
                "start_date": hackathon["start_date"],
                "end_date": hackathon["end_date"],
                "organization_id": hackathon["organization_id"],
                "organization_name": hackathon["organization_name"],
                "location": hackathon["location"],
                "thumbnail": hackathon["thumbnail"],
            }

            result.append(record)

    return flask.jsonify(result)


@web_api.route("/hackathon", methods=["POST"])
@api_util.requires_login(accept_key=True, admin=True)
def create_hackathon(*, user_id):
    # Accept JSON if possible; fall back to form-encoded data
    hackathon_body = flask.request.get_json()
    if not hackathon_body:
        hackathon_body = flask.request.form

    title = hackathon_body["title"]
    description = hackathon_body["description"]
    location = hackathon_body["location"]
    thumbnail = hackathon_body["thumbnail"]
    start_date = arrow.get(hackathon_body["start_date"]).datetime
    end_date = arrow.get(hackathon_body["end_date"]).datetime
    organization_id = hackathon_body.get("organization_id", None)

    if end_date < arrow.now().datetime:
        raise util.APIError(
            400,
            message="End date is in the past!"
        )

    if start_date >= end_date:
        raise util.APIError(
            400,
            message="Start date must be before end date."
        )

    with model.engine.connect() as conn:
        if organization_id is not None:
            organization_id = int(organization_id)
            organization = conn.execute(
                model.organizations.select(
                    model.organizations.c.id == organization_id)
            ).first()

            if not organization:
                raise util.APIError(
                    400,
                    message="Invalid organization ID."
                )

        verification_code = uuid.uuid4().hex
        hackathon_id = conn.execute(model.hackathons.insert().values(
            title=title,
            description=description,
            start_date=start_date,
            end_date=end_date,
            verification_code=verification_code,
            organization_id=organization_id,
            location=location,
            thumbnail = thumbnail,
        )).inserted_primary_key[0]

        return util.response_success({
            "hackathon_id": hackathon_id,
            "verification_code": verification_code,
        }, status_code=201)


@web_api.route("/hackathon/<int:hackathon_id>", methods=["GET"])
@util.cross_origin(methods=["GET", "PUT"])
@api_util.requires_login(accept_key=True)
def get_hackathon(hackathon_id, *, user_id):
    with model.engine.connect() as conn:
        hackathon = conn.execute(hackathon_query.where(
            model.hackathons.c.id == hackathon_id)).first()

        if (not hackathon or
                not can_view_hackathon(user_id, hackathon["id"], conn)):
            raise util.APIError(404)

        hackathon_users = conn.execute(
            model.hackathon_total_ranked_users_query(hackathon_id)
        ).first()[0]

        return flask.jsonify({
            "hackathon_id": hackathon["id"],
            "title": hackathon["title"],
            "description": hackathon["description"],
            "status": api_util.hackathon_status(hackathon["start_date"],
                                                hackathon["end_date"]),
            "start_date": hackathon["start_date"],
            "end_date": hackathon["end_date"],
            "organization_id": hackathon["organization_id"],
            "organization_name": hackathon["organization_name"],
            "num_participants": hackathon_users,
            "location": hackathon["location"],
            "thumbnail": hackathon["thumbnail"],
        })


@web_api.route("/hackathon/<int:hackathon_id>", methods=["PUT"])
@util.cross_origin(methods=["GET", "PUT"])
@api_util.requires_login(admin=True, accept_key=True)
def update_hackathon(hackathon_id, *, user_id):
    values = {}
    hackathon_body = flask.request.get_json()
    if not hackathon_body:
        hackathon_body = flask.request.form

    if "title" in hackathon_body:
        values["title"] = hackathon_body["title"]

    if "description" in hackathon_body:
        values["description"] = hackathon_body["description"]

    if "organization_id" in hackathon_body:
        values["organization_id"] = int(hackathon_body["organization_id"])

    if "location" in hackathon_body:
        values["location"] = hackathon_body["location"]

    if "thumbnail" in hackathon_body:
        values["location"] = hackathon_body["thumbnail"]

    with model.engine.connect() as conn:
        hackathon = conn.execute(
            model.hackathons.select(model.hackathons.c.id == hackathon_id)
        ).first()

        if not hackathon:
            raise util.APIError(
                404,
                message="Hackathon does not exist."
            )

        if "organization_id" in values:
            organization_id = int(values["organization_id"])
            organization = conn.execute(
                model.organizations.select(model.organizations.c.id ==
                                           organization_id)
            ).first()

            if not organization:
                raise util.APIError(
                    400,
                    message="Invalid organization ID."
                )

        if values:
            conn.execute(model.hackathons.update().values(**values).where(
                model.hackathons.c.id == hackathon_id
            ))

        return util.response_success({
            "hackathon_id": hackathon_id,
            "updated_values": values,
        })


@web_api.route("/hackathon/<int:hackathon_id>/leaderboard", methods=["GET"])
@util.cross_origin(methods=["GET"])
@api_util.requires_login(accept_key=True)
def get_hackathon_leaderboard(hackathon_id, *, user_id):
    with model.engine.connect() as conn:
        if not can_view_hackathon(user_id, hackathon_id, conn):
            raise util.APIError(404)

        table = model.hackathon_ranked_bots_users_query(hackathon_id)

        result = []
        offset, limit = api_util.get_offset_limit()

        where_clause, order_clause, _ = api_util.get_sort_filter({
            "user_id": table.c.user_id,
            "username": table.c.username,
            "level": table.c.player_level,
            "organization_id": table.c.organization_id,
            "version_number": table.c.num_submissions,
            "local_rank": table.c.local_rank,
            "language": table.c.language,
        })

        if not order_clause:
            order_clause = [table.c.local_rank]

        hackathon_users = conn.execute(
            model.hackathon_total_ranked_users_query(hackathon_id)).first()[0]

        query = conn.execute(
            table.select()
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
                "score": float(row["score"]),
                "language": row["language"],
                "local_rank": int(row["local_rank"])
                if row["local_rank"] is not None else None,
            }

            if hackathon_users and row["local_rank"] is not None:
                user["local_tier"] = util.tier(
                    row["local_rank"], hackathon_users)
            else:
                user["local_tier"] = None

            result.append(user)

        return flask.jsonify(result)

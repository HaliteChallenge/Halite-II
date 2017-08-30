"""
Badges API endpoints - get/update badges
"""

import flask

from .. import model, util

from . import util as web_util
from .blueprint import web_api
from sqlalchemy import and_, exc


def make_user_badge_record(row):
    user_badge = {
        'user_id': row["user_id"],
        "badge_id":  row["badge_id"],
        "is_enabled": bool(row["is_enabled"]),
        "creation_time":  row["creation_time"],
        "update_time": row["update_time"],
    }
    return user_badge


# Get all badges for a given user
# TODO: add a way to retrieve disabled badges
@web_api.route("/user/<int:user_id>/badge")
def get_user_badges(user_id):
    result = []
    enabled_only = bool(flask.request.args.get('enabled_only', 0, int))
    with model.engine.connect() as conn:
        query = conn.execute(model.user_badge.select().where(
            model.user_badge.c.user_id == user_id
        ))

    for row in query:
        if not enabled_only or row['is_enabled']:
            result.append(make_user_badge_record(row))
    return flask.jsonify(result)


@web_api.route("/user/<int:user_id>/badge/<int:badge_id>")
def get_badge_for_user(user_id, badge_id):
    with model.engine.connect() as conn:
        result = conn.execute(model.user_badge.select().where(
            and_(
                model.user_badge.c.user_id == user_id,
                model.user_badge.c.badge_id == badge_id
            )
        )).first()
    if result is None:
        raise util.APIError(404, message="Badge not found for this user")
    return flask.jsonify(make_user_badge_record(result))


@web_api.route("/user/<int:intended_user_id>/badge", methods=["POST"])
@web_util.requires_login(admin=True, accept_local=True)
def add_badge_for_user(intended_user_id, *, user_id):
    body = flask.request.get_json()
    if "badge_id" not in body:
        raise util.APIError(400, message="Badge_id must be specified.")
    if "is_enabled" not in body:
        body["is_enabled"] = True

    with model.engine.connect() as conn:
        try:
            conn.execute(model.user_badge.insert().values(
                user_id=intended_user_id,
                badge_id=body['badge_id'],
                is_enabled=body['is_enabled'],
                ))
        except exc.IntegrityError as e:
            # (intended_user_id, badbe_id already exists)
            if e.orig.args[0] == 1062:
                raise util.APIError(
                    409,
                    message="Badge already exists for user (may be disabled)",
                )
            raise
    return util.response_success()


@web_api.route("/user/<int:intended_user_id>/badge/<int:badge_id>", methods=["DELETE"])
@web_util.requires_login(admin=True, accept_local=True)
def delete_badge_for_user(intended_user_id, badge_id, *, user_id):
    with model.engine.connect() as conn:
        res = conn.execute(model.user_badge.delete().where(
            and_(
                model.user_badge.c.user_id == intended_user_id,
                model.user_badge.c.badge_id == badge_id
            )))
        if res.rowcount == 0:
            raise util.APIError(404, message="Badge not found for this user")

    return util.response_success()


@web_api.route("/user/<int:intended_user_id>/badge/<int:badge_id>", methods=["PUT"])
@web_util.requires_login(admin=True)
def update_badge_for_user(intended_user_id, badge_id, *, user_id):
    body = flask.request.get_json()
    if  'is_enabled' not in body:
        raise util.APIError(400, message="'is_enabled' should be specified'")
    is_enabled = bool(int(body['is_enabled']))

    with model.engine.connect() as conn:
        res = conn.execute(model.user_badge.update().where(
            and_(
                model.user_badge.c.user_id == intended_user_id,
                model.user_badge.c.badge_id == badge_id
            )).values(is_enabled=is_enabled))
        if res.rowcount == 0:
            raise util.APIError(404, message="Badge not found for this user")

    return util.response_success()


@web_api.route("/badge")
def list_badges():
    result = []
    with model.engine.connect() as conn:
        query = conn.execute(model.badge.select())
        for row in query.fetchall():
            result.append({
                "id" : row["id"],
                "name": row["name"],
                "family": row["family"],
                "family_id": row["family_id"],
                })
    return flask.jsonify(result)


# TODO: ADD URL to schema
@web_api.route("/badge", methods=['POST'])
@web_util.requires_login(admin=True, accept_local=True)
def add_badge(*, user_id):
    badge_id = None
    body = flask.request.get_json()
    for key in body:
        if key not in ['name', 'family', 'family_id']:
            raise util.APIError(400, message="Cannot update '{}'".format(key))
    with model.engine.connect() as conn:
        badge_id = conn.execute(model.badge.insert().values(
            name=body['name'],
            family=body.get('family'),
            family_id=body.get('family_id'),
            )).inserted_primary_key

    return util.response_success({
        "id": badge_id
    }, status_code=201)


@web_api.route("/badge/<int:badge_id>", methods=['PUT'])
@web_util.requires_login(admin=True, accept_local=True)
def update_badge(badge_id, *, user_id):
    body = flask.request.get_json()
    for key in body:
        if key not in ['name', 'family', 'family_id']:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

    with model.engine.connect() as conn:
       res = conn.execute(model.badge.update().values(**body)
            .where( model.badge.c.id == badge_id))

    return util.response_success()

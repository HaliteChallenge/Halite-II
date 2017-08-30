"""
Badges API endpoints - get/update badges
"""

import flask

from .. import model, util

from . import util as web_util
from .blueprint import web_api
from sqlalchemy import and_, exc

_ENABLED_KEY = "is_enabled"
_USER_KEY = "user_id"
_BADGE_KEY = "badge_id"
_CREATION_TIME_KEY = "creation_time"
_UPDATE_TIME_KEY = "update_time"
_NAME_KEY = "name"
_ID_KEY = "id"
_MYSQL_ER_DUP_ENTRY = 1062

def make_user_badge_record(row):
    user_badge = {
        _USER_KEY: row[_USER_KEY],
        _BADGE_KEY:  row[_BADGE_KEY],
        _ENABLED_KEY: bool(row[_ENABLED_KEY]),
        _CREATION_TIME_KEY:  row[_CREATION_TIME_KEY],
        _UPDATE_TIME_KEY: row[_UPDATE_TIME_KEY],
    }
    return user_badge


@web_api.route("/user/<int:user_id>/badge")
def get_user_badges(user_id):
    """Get all badge a users has. By default only enabled ones are returned."""
    result = []
    enabled_only = bool(flask.request.args.get('enabled_only', 0, int))
    with model.engine.connect() as conn:
        query = conn.execute(model.user_badge.select().where(
            model.user_badge.c.user_id == user_id
        ))

    for row in query:
        if not enabled_only or row[_ENABLED_KEY]:
            result.append(make_user_badge_record(row))
    return flask.jsonify(result)


@web_api.route("/user/<int:user_id>/badge/<int:badge_id>")
def get_badge_for_user(user_id, badge_id):
    """Given a user and an ID return the badge metadata."""
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
    """Assign user a badge.

    Arguemnts:
    is_enabled -- default 0 (False)
    badge_id -- the id of the badge in the badge table
    """
    body = flask.request.get_json()
    if _BADGE_KEY not in body:
        raise util.APIError(400, message="Badge_id must be specified.")
    if _ENABLED_KEY not in body:
        body[_ENABLED_KEY] = True

    with model.engine.connect() as conn:
        try:
            conn.execute(model.user_badge.insert().values(
                user_id=intended_user_id,
                badge_id=body[_BADGE_KEY],
                is_enabled=body.get(_ENABLED_KEY, True)
                ))
        except exc.IntegrityError as e:
            # (intended_user_id, badbe_id already exists)
            if e.orig.args[0] == _MYSQL_ER_DUP_ENTRY:
                raise util.APIError(
                    409,
                    message="Badge already exists for user (may be disabled)",
                )
            raise
    return util.response_success()


@web_api.route("/user/<int:intended_user_id>/badge/<int:badge_id>", methods=["DELETE"])
@web_util.requires_login(admin=True, accept_local=True)
def delete_badge_for_user(intended_user_id, badge_id, *, user_id):
    """Delete badge_id badge from user_id user"""
    with model.engine.connect() as conn:
        res = conn.execute(model.user_badge.delete().where(
            and_(
                model.user_badge.c.user_id == intended_user_id,
                model.user_badge.c.badge_id == badge_id
            )))
        if not res.rowcount:
            raise util.APIError(404, message="Badge not found for this user")

    return util.response_success()


@web_api.route("/user/<int:intended_user_id>/badge/<int:badge_id>", methods=["PUT"])
@web_util.requires_login(admin=True)
def update_badge_for_user(intended_user_id, badge_id, *, user_id):
    """Change user_badge metadata: enable/disable."""
    body = flask.request.get_json()
    if  _ENABLED_KEY not in body:
        raise util.APIError(400, message="'{}'should be specified'".format(_ENABLED_KEY))
    is_enabled = bool(int(body[_ENABLED_KEY]))

    with model.engine.connect() as conn:
        res = conn.execute(model.user_badge.update().where(
            and_(
                model.user_badge.c.user_id == intended_user_id,
                model.user_badge.c.badge_id == badge_id
            )).values(is_enabled=is_enabled))
        if not res.rowcount:
            raise util.APIError(404, message="Badge not found for this user")

    return util.response_success()


@web_api.route("/badge")
def list_badges():
    """Returns a list of all assignable badges."""
    result = []
    with model.engine.connect() as conn:
        query = conn.execute(model.badge.select())
        for row in query.fetchall():
            result.append({
                _ID_KEY : row[_ID_KEY],
                _NAME_KEY: row[_NAME_KEY],
                })
    return flask.jsonify(result)


# TODO: ADD URL to schema
@web_api.route("/badge", methods=['POST'])
@web_util.requires_login(admin=True, accept_local=True)
def add_badge(*, user_id):
    """Add a new badge that can be assigned. Name must be unique."""
    badge_id = None
    body = flask.request.get_json()
    for key in body:
        if key not in [_NAME_KEY]:
            raise util.APIError(400, message="Cannot update '{}'".format(key))
    with model.engine.connect() as conn:
        badge_id = conn.execute(model.badge.insert().values(
            name=body[_NAME_KEY],
            )).inserted_primary_key

    return util.response_success({
        _ID_KEY: badge_id
    }, status_code=201)


@web_api.route("/badge/<int:badge_id>", methods=['PUT'])
@web_util.requires_login(admin=True, accept_local=True)
def update_badge(badge_id, *, user_id):
    """Change metadata for a badge"""
    body = flask.request.get_json()
    for key in body:
        if key not in [_NAME_KEY]:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

    with model.engine.connect() as conn:
       res = conn.execute(model.badge.update().values(**body)
            .where( model.badge.c.id == badge_id))

    return util.response_success()

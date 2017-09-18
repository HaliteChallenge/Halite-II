import datetime
import functools
import operator

import arrow
import flask
import sqlalchemy
import pycountry

from .. import config, model, util


def validate_country(country_code, subdivision_code):
    try:
        country = pycountry.countries.get(alpha_3=country_code)
    except KeyError:
        return False

    if subdivision_code:
        subdivisions = pycountry.subdivisions.get(country_code=country.alpha_2)
        for subdivision in subdivisions:
            if subdivision.code == subdivision_code:
                return True
        return False
    else:
        return True


def validate_user_level(level):
    return level in ('High School', 'Undergraduate', 'Graduate',
                     'Professional')


def validate_api_key(api_key):
    """
    Validate the given API key and retrieve the corresponding user record.

    :raises: util.APIError if the key is invalid
    """
    if not api_key:
        return None

    if ":" not in api_key:
        raise util.APIError(403, message="API key is in invalid format.")

    user_id, api_key = api_key.split(":", 1)
    user_id = int(user_id)
    with model.engine.connect() as conn:
        user = conn.execute(sqlalchemy.sql.select([
            model.users.c.id.label("user_id"),
            model.users.c.is_admin,
            model.users.c.api_key_hash,
            model.users.c.is_email_good,
            model.users.c.is_active,
        ]).where(model.users.c.id == user_id)).first()

        if not user:
            return None

        if config.api_key_context.verify(api_key, user["api_key_hash"]):
            return user

    return None


def validate_session_cookie(user_id):
    """
    Validate the session cookie and retrieve the corresponding user record.
    """
    with model.engine.connect() as conn:
        user = conn.execute(sqlalchemy.sql.select([
            model.users.c.id.label("user_id"),
            model.users.c.is_admin,
            model.users.c.api_key_hash,
            model.users.c.is_email_good,
            model.users.c.is_active,
        ]).where(model.users.c.id == user_id)).first()

        return user


def requires_login(accept_key=False, optional=False, admin=False,
                   association=False):
    """
    Indicates that an endpoint requires the user to be logged in.

    :param accept_key: if True, then accept an API key, otherwise only accept
    a session cookie (OAuth).
    :param optional: if True, do not return HTTP 403 if the user is not
    logged in.
    :param admin: if True, only accept admin users.
    :param association: if True, only accept users that have associated and
    verified their email.
    """
    def _requires_login(view):
        @functools.wraps(view)
        def decorated_view(*args, **kwargs):
            user = None
            if accept_key:
                user = validate_api_key(
                    flask.request.headers.get(config.API_KEY_HEADER))

            if not user:
                user = validate_session_cookie(
                    flask.session.get(config.SESSION_COOKIE))

            if user:
                if association and not (user["is_email_good"] and
                                        user["is_active"]):
                    raise util.APIError(
                        403,
                        message="Please verify your email first.")

                if admin and user["is_admin"]:
                    kwargs["user_id"] = user["user_id"]
                elif admin:
                    raise util.APIError(
                        403, message="User cannot take this action.")
                else:
                    kwargs["user_id"] = user["user_id"]
            elif optional:
                kwargs["user_id"] = None
            else:
                raise util.APIError(403, message="User not logged in.")

            return view(*args, **kwargs)

        return decorated_view

    return _requires_login


def requires_competition_open(view):
    """Indicates that an endpoint requires the competition is running."""
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if not config.COMPETITION_OPEN:
            raise util.APIError(
                403,
                message="Sorry, the competition has ended. Thanks for playing!")
        return view(*args, **kwargs)

    return decorated_view


def is_user_admin(user_id, *, conn):
    user = conn.execute(model.users.select(model.users.c.id == user_id)).first()
    return user and user["is_admin"]


def user_mismatch_error(message="Cannot perform action for other user."):
    """
    Error for when the logged in and target users of an action don't match.
    """
    raise util.APIError(400, message=message)


def get_offset_limit(*, default_limit=50, max_limit=250):
    """Get an offset/limit from the query string (or default)."""
    offset = int(flask.request.values.get("offset", 0))
    offset = max(offset, 0)
    limit = int(flask.request.values.get("limit", default_limit))
    limit = min(max(limit, 0), max_limit)

    return offset, limit


def parse_filter(filter_string):
    """
    Parse a filter string into a field name, comparator, and value.
    :param filter_string: Of the format field,operator,value.
    :return: (field_name, operator_func, value)
    """
    field, cmp, value = filter_string.split(",")

    operation = {
        "=": operator.eq,
        "<": operator.lt,
        "<=": operator.le,
        ">": operator.gt,
        ">=": operator.ge,
        "!=": operator.ne,
    }.get(cmp, None)
    if operation is None:
        raise util.APIError(
            400, message="Cannot compare '{}' by '{}'".format(field, cmp))

    return field, operation, value


def get_sort_filter(fields, false_fields=()):
    """
    Parse flask.request to create clauses for SQLAlchemy's order_by and where.

    :param fields: A dictionary of field names to SQLAlchemy table columns
    listing what fields can be sorted/ordered on.
    :param false_fields: A list of fields that can be sorted/ordered on, but
    that the caller will manually handle. (Non-recognized fields generate
    an API error.)
    :return: A 2-tuple of (where_clause, order_clause). order_clause is an
    ordered list of columns.
    """
    where_clause = sqlalchemy.true()
    order_clause = []
    manual_fields = []

    for filter_param in flask.request.args.getlist("filter"):
        field, operation, value = parse_filter(filter_param)

        if field not in fields and field not in false_fields:
            raise util.APIError(
                400, message="Cannot filter on field {}".format(field))

        if field in false_fields:
            manual_fields.append((field, operation, value))
            continue

        column = fields[field]
        if isinstance(column.type, sqlalchemy.types.Integer):
            conversion = int
        elif isinstance(column.type, sqlalchemy.types.DateTime):
            conversion = lambda x: arrow.get(x).datetime
        elif isinstance(column.type, sqlalchemy.types.Float):
            conversion = float
        elif isinstance(column.type, sqlalchemy.types.String):
            conversion = lambda x: x
        else:
            raise RuntimeError("Filtering on column is not supported yet: " + repr(column))

        value = conversion(value)

        clause = operation(column, value)
        where_clause &= clause

    for order_param in flask.request.args.getlist("order_by"):
        direction = "asc"
        if "," in order_param:
            direction, field = order_param.split(",")
        else:
            field = order_param

        if field not in fields:
            raise util.APIError(
                400, message="Cannot order on field {}".format(field))

        column = fields[field]
        if direction == "asc":
            column = column.asc()
        elif direction == "desc":
            column = column.desc()
        else:
            raise util.APIError(
                400, message="Cannot order column by '{}'".format(direction))

        order_clause.append(column)

    return where_clause, order_clause, manual_fields


def hackathon_status(start_date, end_date):
    """
    Return the status of the hackathon based on its start/end dates.

    `end_date` may be null (for ongoing hackathons).
    """
    status = "open"
    if end_date and end_date < datetime.datetime.now():
        status = "closed"
    elif start_date > datetime.datetime.now():
        status = "upcoming"
    return status

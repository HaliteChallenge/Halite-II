import base64
import functools
import hashlib
import hmac
import io
import logging
import operator
import random
import string
import urllib.parse
import uuid
import zipfile

import arrow
import flask
import sqlalchemy
import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions
import pycountry
from flask_cors import cross_origin as flask_cross_origin

from .. import config, model, notify, util
from .. import response_success


web_api = flask.Blueprint("web_api", __name__)


def cross_origin(*args, **kwargs):
    kwargs["origins"] = config.CORS_ORIGINS
    kwargs["supports_credentials"] = True
    kwargs["allow_headers"] = ["Origin", "Accept", "Content-Type"]
    return flask_cross_origin(*args, **kwargs)


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


def requires_login(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "api_key" in flask.request.args:
            api_key = flask.request.args["api_key"]
            if ":" not in api_key:
                raise util.APIError(401, message="Invalid API key.")

            user_id, api_key = api_key.split(":", 1)
            user_id = int(user_id)
            with model.engine.connect() as conn:
                user = conn.execute(sqlalchemy.sql.select([
                    model.users.c.api_key_hash,
                ]).where(model.users.c.id == user_id)).first()

                if not user:
                    raise util.APIError(401, message="User not logged in.")

                if config.api_key_context.verify(api_key, user["api_key_hash"]):
                    kwargs["user_id"] = user_id
                    return view(*args, **kwargs)
                else:
                    raise util.APIError(401, message="User not logged in.")
        else:
            return requires_oauth_login(view)(*args, **kwargs)

    return decorated_view


def requires_oauth_login(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" not in flask.session:
            raise util.APIError(401, message="User not logged in.")

        with model.engine.connect() as conn:
            user = conn.execute(model.users.select(
                model.users.c.id == flask.session["user_id"])).first()
            if not user:
                raise util.APIError(401, message="User not logged in.")

        kwargs["user_id"] = flask.session["user_id"]
        return view(*args, **kwargs)

    return decorated_view


def optional_login(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" in flask.session:
            kwargs["user_id"] = flask.session["user_id"]
        else:
            kwargs["user_id"] = None
        return view(*args, **kwargs)

    return decorated_view


def requires_admin(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" not in flask.session:
            raise util.APIError(401, message="User not logged in.")
        with model.engine.connect() as conn:
            user_id = flask.session["user_id"]
            user = conn.execute(
                model.users.select(model.users.c.id == user_id)).first()
            if not user or not user["is_admin"]:
                raise util.APIError(
                    401, message="User cannot take this action.")
        return view(*args, **kwargs)

    return decorated_view


def requires_association(view):
    """Indicates that an endpoint requires the user to confirm their email."""
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        user_id = kwargs.get("user_id")
        with model.engine.connect() as conn:
            user = conn.execute(
                model.users.select(model.users.c.id == user_id)).first()
            if not user or not user["is_email_good"] or not user["is_active"]:
                raise util.APIError(
                    400, message="Please verify your email first.")
        return view(*args, **kwargs)

    return decorated_view


def user_mismatch_error(message="Cannot perform action for other user."):
    raise util.APIError(400, message=message)


def get_offset_limit(*, default_limit=50, max_limit=250):
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

######################
# USER API ENDPOINTS #
######################


def make_user_record(row, *, logged_in, total_users=None):
    """Given a database result row, create the JSON user object."""
    user = {
        "user_id": row["user_id"],
        "username": row["username"],
        "level": row["player_level"],
        "organization_id": row["organization_id"],
        "organization": row["organization_name"],
        "country_code": row["country_code"],
        "country_subdivision_code": row["country_subdivision_code"],
        "num_bots": row["num_bots"],
        "num_submissions": int(row["num_submissions"]),
        "num_games": int(row["num_games"]),
        "score": float(row["score"]),
        "rank": int(row["rank"]) if row["rank"] is not None else None,
    }

    if total_users and row["rank"] is not None:
        user["tier"] = util.tier(row["rank"], total_users)
    else:
        user["tier"] = None

    if row["email"] is None and logged_in:
        # User is new user, indicate this when they are logged in
        user["is_new_user"] = True

    return user


@web_api.route("/user")
@cross_origin(methods=["GET", "POST"])
def list_users():
    result = []
    offset, limit = get_offset_limit()

    where_clause, order_clause, _ = get_sort_filter({
        "user_id": model.all_users.c.user_id,
        "username": model.all_users.c.username,
        "level": model.all_users.c.player_level,
        "organization_id": model.all_users.c.organization_id,
        "num_bots": model.all_users.c.num_bots,
        "num_submissions": model.all_users.c.num_submissions,
        "num_games": model.all_users.c.num_games,
        "rank": model.all_users.c.rank,
    })

    with model.engine.connect() as conn:
        total_users = conn.execute(model.total_ranked_bots).first()[0]

        query = conn.execute(
            model.all_users.select()
                    .where(where_clause).order_by(*order_clause)
                    .offset(offset).limit(limit).reduce_columns())

        for row in query.fetchall():
            result.append(make_user_record(row, logged_in=False,
                                           total_users=total_users))

    return flask.jsonify(result)


@web_api.route("/user", methods=["POST"])
@cross_origin(methods=["GET", "POST"])
@requires_login
def create_user(*, user_id):
    """
    Set up a user created from an OAuth authorization flow.

    This endpoint, given an organization ID, generates a validation email
    and sets up the user's actual email.
    """
    body = flask.request.get_json()
    if not body:
        raise util.APIError(400, message="Please provide user data.")

    # Check if the user has already validated
    with model.engine.connect() as conn:
        user_data = conn.execute(model.users.select().where(
            model.users.c.id == user_id
        )).first()

        if user_data["verification_code"]:
            raise util.APIError(400, message="User needs to verify email.")

        if user_data["is_email_good"] == 1:
            raise util.APIError(400, message="User already validated.")

    org_id = body.get("organization_id")
    email = body.get("email")
    level = body.get("level", model.users.c.player_level)
    verification_code = uuid.uuid4().hex

    # Values to insert into the database
    values = {}

    # Perform validation on given values
    if "level" in body and not validate_user_level(body["level"]):
        raise util.APIError(400, message="Invalid user level.")

    if email is not None and "@" not in email:
        raise util.APIError(400, message="Invalid user email.")

    if "country_code" in body or "country_subdivision_code" in body:
        country_code = body.get("country_code")
        subdivision_code = body.get("country_subdivision_code")

        if subdivision_code and not country_code:
            raise util.APIError(400, message="Must provide country code if country subdivision code is given.")

        if not validate_country(country_code, subdivision_code):
            raise util.APIError(400, message="Invalid country/country subdivision code.")

        values["country_code"] = country_code
        values["country_subdivision_code"] = subdivision_code

    # Figure out the situation with their email/organization
    if org_id is None and email is None:
        # Just use their Github email. Don't bother guessing an affiliation
        # (we can do that client-side)
        values.update({
            "email": model.users.c.github_email,
            "is_email_good": 1,
            "organization_id": None,
            "player_level": level,
        })
    elif org_id is None:
        values.update({
            "email": email,
            "is_email_good": 0,
            "verification_code": verification_code,
            "organization_id": None,
            "player_level": level,
        })
    else:
        # Check the org
        with model.engine.connect() as conn:
            org = conn.execute(model.organizations.select().where(
                model.organizations.c.id == org_id
            )).first()

            if org is None:
                raise util.APIError(400, message="Organization does not exist.")

            # Verify the email against the org
            email_to_verify = email or user_data["github_email"]
            if not email or "@" not in email_to_verify:
                raise util.APIError(
                    400, message="Email invalid.")
            domain = email.split("@")[1].strip().lower()
            count = conn.execute(sqlalchemy.sql.select([
                sqlalchemy.sql.func.count()
            ]).select_from(model.organization_email_domains).where(
                (model.organization_email_domains.c.organization_id == org_id) &
                (model.organization_email_domains.c.domain == domain)
            )).first()[0]

            if count == 0:
                raise util.APIError(
                    400, message="Invalid email for organization.")

    # Set the verification code (if necessary).
    if email:
        values.update({
            "email": email,
            "is_email_good": 0,
            "verification_code": verification_code,
            "organization_id": org_id,
        })

        notify.send_notification(
            email,
            user_data["username"],
            "Email verification",
            notify.VERIFY_EMAIL.format(
                user_id=user_id,
                verification_code=verification_code),
        )

        message = "Please check your email for a verification code."
    else:
        values.update({
            "email": model.users.c.github_email,
            "is_email_good": 1,
            "organization_id": org_id,
        })
        message = "You've been added to the organization automatically!"

    if org_id is None:
        raise util.APIError(
            401,
            message="For the alpha, you must be associated with an organization.")

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(**values))

    return response_success({
        "message": message,
    })


@web_api.route("/user/<int:intended_user>", methods=["GET"])
# TODO: need CSRF protection
@cross_origin(methods=["GET"])
@optional_login
def get_user(intended_user, *, user_id):
    with model.engine.connect() as conn:
        query = model.all_users.select(
            model.all_users.c.user_id == intended_user)

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        total_users = conn.execute(model.total_ranked_bots).first()[0]

        logged_in = user_id is not None and intended_user == user_id
        return flask.jsonify(make_user_record(row, logged_in=logged_in,
                                              total_users=total_users))


@web_api.route("/user/<int:user_id>/verify", methods=["POST"])
@cross_origin(methods=["POST"])
def verify_user_email(user_id):
    verification_code = flask.request.form.get("verification_code")
    if not verification_code:
        raise util.APIError(400, message="Please provide verification code.")

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.users.c.verification_code,
            model.users.c.is_email_good,
        ]).where(model.users.c.id == user_id)

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        if row["verification_code"] == verification_code:
            conn.execute(model.users.update().where(
                model.users.c.id == user_id
            ).values(
                is_email_good=1,
                verification_code="",
            ))
            return response_success({
                "message": "Email verified."
            })
        elif row["is_email_good"]:
            return response_success({
                "message": "Email already verified.",
            })

        raise util.APIError(400, message="Invalid verification code.")


@web_api.route("/user/<int:intended_user_id>", methods=["PUT"])
@requires_login
def update_user(intended_user_id, *, user_id):
    if user_id != intended_user_id:
        raise user_mismatch_error()

    fields = flask.request.get_json()
    columns = {
        "level": "player_level",
        "country_code": "country_code",
        "country_subdivision_code": "country_subdivision_code",
    }

    update = {}

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

        update[columns[key]] = fields[key]

    if "player_level" in update and not validate_user_level(update["player_level"]):
        raise util.APIError(400, message="Invalid player level.")

    if "country_code" in update or "country_subdivision_code" in update:
        with model.engine.connect() as conn:
            user = conn.execute(model.users.select(model.users.c.id == user_id)).first()
        country_code = update.get("country_code", user["country_code"])
        subdivision_code = update.get("country_subdivision_code", user["country_subdivision_code"])

        if not validate_country(country_code, subdivision_code):
            raise util.APIError(400, message="Invalid country/country subdivision code.")

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(**update))

    return response_success()


@web_api.route("/user/<int:intended_user_id>", methods=["DELETE"])
@requires_admin
def delete_user(intended_user_id, *, user_id):
    # TODO: what happens to their games?
    if user_id != intended_user_id:
        raise user_mismatch_error()

    with model.engine.connect() as conn:
        conn.execute(model.users.delete().where(
            model.users.c.id == user_id))


# ---------------------- #
# USER BOT API ENDPOINTS #
# ---------------------- #

# Currently, there is no notion of a user having multiple distinct bots.
# However, in the API, we pretend this is the case as much as possible, since
# we may wish to support this eventually.


@web_api.route("/user/<int:user_id>/bot", methods=["GET"])
@cross_origin(methods=["GET"])
def list_user_bots(user_id):
    result = []
    with model.engine.connect() as conn:
        bots = conn.execute(sqlalchemy.sql.select([
            model.bots.c.id,
            model.bots.c.version_number,
            model.bots.c.games_played,
            model.bots.c.language,
            model.bots.c.score,
            model.bots.c.compile_status,
            sqlalchemy.sql.text("ranked_bots.bot_rank"),
        ]).select_from(
            model.bots.join(
                model.ranked_bots,
                (model.bots.c.id == model.ranked_bots.c.bot_id) &
                (model.bots.c.user_id == model.ranked_bots.c.user_id)
            )
        ).where(
            model.bots.c.user_id == user_id
        ).order_by(model.bots.c.id)).fetchall()

        for bot in bots:
            result.append({
                "bot_id": bot["id"],
                "rank": int(bot["bot_rank"]) if bot["bot_rank"] else None,
                "version_number": bot["version_number"],
                "games_played": bot["games_played"],
                "language": bot["language"],
                "score": bot["score"],
                "compilation_status": bot["compile_status"],
            })

    return flask.jsonify(result)


@web_api.route("/user/<int:user_id>/bot/<int:bot_id>", methods=["GET"])
@cross_origin(methods=["GET", "PUT"])
def get_user_bot(user_id, bot_id):
    with model.engine.connect() as conn:
        bot = conn.execute(sqlalchemy.sql.select([
            model.bots.c.id,
            model.bots.c.version_number,
            model.bots.c.games_played,
            model.bots.c.language,
            model.bots.c.score,
            model.bots.c.compile_status,
            sqlalchemy.sql.text("ranked_bots.bot_rank"),
        ]).select_from(
            model.bots.join(
                model.ranked_bots,
                (model.bots.c.id == model.ranked_bots.c.bot_id) &
                (model.bots.c.user_id == model.ranked_bots.c.user_id)
            )
        ).where(
            (model.bots.c.user_id == user_id) &
            (model.bots.c.id == bot_id)
        ).order_by(model.bots.c.id)).first()

        if not bot:
            raise util.APIError(404, message="Bot not found.")

        return flask.jsonify({
            "bot_id": bot_id,
            "rank": int(bot["bot_rank"]) if bot["bot_rank"] else None,
            "version_number": bot["version_number"],
            "games_played": bot["games_played"],
            "language": bot["language"],
            "score": bot["score"],
            "compilation_status": bot["compile_status"],
        })


def validate_bot_submission():
    """Validate the uploaded bot, returning the bot file if so."""
    if not config.COMPETITION_OPEN:
        raise util.APIError(
            400, message="Sorry, but bot submissions are closed."
        )

    if "botFile" not in flask.request.files:
        raise util.APIError(400, message="Bot file not provided (must "
                            "provide as botFile).")

    # Save to GCloud
    uploaded_file = flask.request.files["botFile"]
    if not zipfile.is_zipfile(uploaded_file):
        raise util.APIError(
            400,
            message="Bot file does not appear to be a zip file. Please "
                    "upload a zip file containing your bot, where the "
                    "main file is named MyBot with an appropriate "
                    "extension.")

    uploaded_file.seek(0)
    return uploaded_file


@web_api.route("/user/<int:intended_user>/bot", methods=["POST"])
@cross_origin(methods=["POST"])
@requires_login
@requires_association
def create_user_bot(intended_user, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot create bot for another user.")

    _ = validate_bot_submission()

    with model.engine.connect() as conn:
        if conn.execute(model.bots.select(model.bots.c.user_id == user_id)).first():
            raise util.APIError(
                400, message="Only one bot allowed per user.")

        conn.execute(model.bots.insert().values(
            user_id=user_id,
            id=0,
            compile_status="Failed",
        ))

    store_user_bot(intended_user=intended_user, user_id=user_id, bot_id=0)
    return response_success({
        "bot_id": 0,
    })


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["PUT"])
@cross_origin(methods=["GET", "PUT"])
@requires_login
@requires_association
def store_user_bot(user_id, intended_user, bot_id):
    """Store an uploaded bot in object storage."""
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot upload bot for another user.")

    if bot_id != 0:
        raise util.APIError(
            400, message="Sorry, only one bot allowed per user.")

    uploaded_file = validate_bot_submission()

    bot_where_clause = (model.bots.c.user_id == user_id) & \
                       (model.bots.c.id == bot_id)
    with model.engine.connect() as conn:
        bot = conn.execute(model.bots.select(bot_where_clause)).first()
        if not bot:
            raise util.APIError(404, message="Bot not found.")

        # Check if the user already has a bot compiling
        if bot["compile_status"] not in ("Successful", "Failed", "Disabled"):
            raise util.APIError(400, message="Cannot upload new bot until "
                                             "previous one is compiled.")

        blob = gcloud_storage.Blob("{}_{}".format(user_id, bot_id),
                                   model.get_compilation_bucket(),
                                   chunk_size=262144)
        blob.upload_from_file(uploaded_file)

        # Flag the user as compiling
        update = model.bots.update() \
            .where(bot_where_clause) \
            .values(
                compile_status="Uploaded",
                update_time=sqlalchemy.sql.func.now(),
            )
        conn.execute(update)

    # TODO: Email the user

    return response_success()


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["DELETE"])
@requires_login
@requires_association
def delete_user_bot(intended_user, bot_id, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot delete bot for another user.")

    with model.engine.connect() as conn:
        # TODO: move bot to BotHistory
        conn.execute(model.bots.delete().where(
            (model.bots.c.user_id == user_id) &
            (model.bots.c.id == bot_id)
        ))

        for bucket in [model.get_bot_bucket(), model.get_compilation_bucket()]:
            try:
                blob = gcloud_storage.Blob(str(user_id), bucket)
                blob.delete()
            except gcloud_exceptions.NotFound:
                pass

        return response_success()


@web_api.route("/user/<int:intended_user>/api_key", methods=["POST"])
@requires_oauth_login
@requires_association
def reset_api_key(intended_user, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot reset another user's API key.")

    chars = string.ascii_letters + string.digits
    with model.engine.connect() as conn:
        api_key = "".join(random.choice(chars) for _ in range(32))

        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(
            api_key_hash=config.api_key_context.hash(api_key),
        ))

        return response_success({
            "api_key": "{}:{}".format(user_id, api_key),
        })


# ------------------------ #
# USER MATCH API ENDPOINTS #
# ------------------------ #
@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
@cross_origin(methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = get_offset_limit()
    where_clause, order_clause, manual_sort = get_sort_filter({
        "game_id": model.games.c.id,
        "time_played": model.games.c.time_played,
        # TODO: filter by participants
    }, ["timed_out"])
    result = []

    participant_clause = sqlalchemy.true()
    for (field, _, _) in manual_sort:
        if field == "timed_out":
            participant_clause &= model.game_participants.c.timed_out

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
            (model.game_participants.c.user_id == intended_user) &
            participant_clause,
        )).where(where_clause).order_by(*order_clause).offset(offset).limit(limit).reduce_columns()
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

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>", methods=["GET"])
@cross_origin(methods=["GET"])
def get_user_match(intended_user, match_id):
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

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/replay",
               methods=["GET"])
@cross_origin(methods=["GET"])
def get_match_replay(intended_user, match_id):
    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replay_name,
        ]).where(
            model.games.c.id == match_id
        )).first()

        blob = gcloud_storage.Blob(match["replay_name"],
                                   model.get_replay_bucket(),
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
@requires_login
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


##############################
# ORGANIZATION API ENDPOINTS #
##############################
@web_api.route("/organization")
@cross_origin(methods=["GET"])
def list_organizations():
    result = []
    offset, limit = get_offset_limit()
    where_clause, order_clause, _ = get_sort_filter({
        "organization_id": model.organizations.c.id,
        "name": model.organizations.c.organization_name,
        "type": model.organizations.c.kind,
    })

    with model.engine.connect() as conn:
        # Don't limit this query
        query = model.organizations.select()\
            .where(where_clause).order_by(*order_clause)\
            .offset(offset)
        orgs = conn.execute(query)

        for org in orgs.fetchall():
            result.append({
                "organization_id": org["id"],
                "name": org["organization_name"],
                "type": org["kind"],
            })

    return flask.jsonify(result)


@web_api.route("/organization/<int:org_id>", methods=["GET"])
@cross_origin(methods=["GET"])
def get_organization(org_id):
    with model.engine.connect() as conn:
        org = conn.execute(model.organizations.select().where(
            model.organizations.c.id == org_id
        )).first()

        if not org:
            raise util.APIError(404, message="Organization not found.")

        return flask.jsonify({
            "organization_id": org["id"],
            "name": org["organization_name"],
            "type": org["kind"],
        })


@web_api.route("/organization", methods=["POST"])
@requires_admin
def create_organization():
    org_body = flask.request.get_json()
    if "name" not in org_body:
        raise util.APIError(400, message="Organization must be named.")

    if "type" not in org_body:
        raise util.APIError(400, message="Organization must have a type.")

    with model.engine.connect() as conn:
        org_id = conn.execute(model.organizations.insert().values(
            organization_name=org_body["name"],
            organization_kind=org_body["type"],
        )).inserted_primary_key

    return response_success({
        "organization_id": org_id[0],
    })


@web_api.route("/organization/<int:org_id>", methods=["PUT"])
@requires_admin
def update_organization(org_id):
    fields = flask.request.get_json()
    columns = {
        "name": model.organizations.c.organization_name,
        "type": model.organizations.c.kind,
    }

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

    with model.engine.connect() as conn:
        conn.execute(model.organizations.update().where(
            model.organizations.c.id == org_id
        ).values(**fields))

    return response_success()


@web_api.route("/organization/<int:org_id>", methods=["DELETE"])
@requires_admin
def delete_organization(org_id):
    with model.engine.connect() as conn:
        with conn.begin() as transaction:
            count = conn.execute(sqlalchemy.sql.select([
                sqlalchemy.sql.func.count()
            ]).select_from(model.users).where(
                model.users.c.id == org_id
            )).first()[0]

            if count > 0:
                raise util.APIError(
                    400, message="Cannot delete organization with members.")

            conn.execute(model.organizations.delete().where(
                model.organizations.c.id == org_id
            ))

    return response_success()

############################
# MATCH DATA API ENDPOINTS #
############################

@web_api.route("/latestMatch")
def list_matches():
    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.id,
            model.games.c.replay_name,
        ]).order_by(model.games.c.timestamp.desc())).first()

        blob = gcloud_storage.Blob(match["replayName"],
                                   model.get_replay_bucket(),
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(buffer, mimetype="application/x-halite-2-replay",
                               as_attachment=True,
                               attachment_filename=str(match["id"])+".hlt")


@web_api.route("/match/<int:match_id>")
@optional_login
def get_match(match_id, *, user_id):
    if user_id is None:
        user_id = -1
    return get_user_match(user_id, match_id, user_id=user_id)


#############################
# LEADERBOARD API ENDPOINTS #
#############################


@web_api.route("/leaderboard")
@cross_origin(methods=["GET"])
def leaderboard():
    result = []
    offset, limit = get_offset_limit()

    where_clause, order_clause, _ = get_sort_filter({
        "user_id": model.ranked_bots_users.c.user_id,
        "username": model.ranked_bots_users.c.username,
        "level": model.ranked_bots_users.c.player_level,
        "organization_id": model.ranked_bots_users.c.organization_id,
        "version_number": model.ranked_bots_users.c.num_submissions,
        "num_games": model.ranked_bots_users.c.num_games,
        "rank": model.ranked_bots_users.c.rank,
        "language": model.ranked_bots_users.c.language,
    })

    if not order_clause:
        order_clause = [model.ranked_bots_users.c.rank]

    with model.engine.connect() as conn:
        total_users = conn.execute(model.total_ranked_bots).first()[0]

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


@web_api.route("/login/discourse_sso")
@requires_login
def discourse_sso(*, user_id):
    """
    Implements an SSO endpoint for Discourse forums, as described at
    https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045
    """

    sso_payload = flask.request.args.get("sso")
    sso_signature = flask.request.args.get("sig")
    if not sso_payload or not sso_signature:
        raise util.APIError(400, message="SSO payload and signature required.")

    hmac_obj = hmac.new(config.DISCOURSE_SSO_SECRET,
                        msg=sso_payload,
                        digestmod=hashlib.sha256)
    computed_signature = hmac_obj.hexdigest()
    if computed_signature != sso_signature:
        raise util.APIError(401)

    with model.engine.connect() as conn:
        user = conn.execute(model.users.select().where(
            model.users.c.id == user_id,
        )).first()

        if not user:
            raise util.APIError(401)

    payload = urllib.parse.parse_qs(
        base64.b64decode(urllib.parse.unquote(sso_payload)))
    nonce = payload.get(b"nonce")
    if not nonce:
        raise util.APIError(401)
    nonce = nonce[0]

    raw_payload = {
        "nonce": nonce,
        "email": user["email"],
        "external_id": str(user_id),
        "username": user["username"],
    }
    if user["is_email_good"] != 1:
        raw_payload["require_activation"] = "true"

    encoded_payload = base64.b64encode(
        urllib.parse.urlencode(raw_payload).encode("utf-8")).decode("utf-8")
    new_signature = hmac.new(config.DISCOURSE_SSO_SECRET,
                             msg=encoded_payload,
                             digestmod=hashlib.sha256).hexdigest()
    return flask.redirect(
        config.DISCOURSE_URL +
        "?sso={}&sig={}".format(encoded_payload, new_signature))

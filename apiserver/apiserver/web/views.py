import base64
import functools
import hashlib
import hmac
import io
import operator
import random
import string
import urllib.parse

import arrow
import flask
import sqlalchemy
import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions
import pycountry

from .. import config, model, util
from .. import response_success


web_api = flask.Blueprint("web_api", __name__)


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
    return level in ('High School','Undergraduate','Graduate','Professional')


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
                    model.users.c.apiKeyHash,
                ]).where(model.users.c.userID == user_id)).first()

                if not user:
                    raise util.APIError(401, message="User not logged in.")

                if config.api_key_context.verify(api_key, user["apiKeyHash"]):
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
    # TODO: NOT IMPLEMENTED!
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        return view(*args, **kwargs)

    return decorated_view


def user_mismatch_error(message="Cannot perform action for other user."):
    raise util.APIError(400, message=message)


def get_offset_limit(*, default_limit=10, max_limit=100):
    offset = int(flask.request.values.get("offset", 0))
    offset = max(offset, 0)
    limit = int(flask.request.values.get("limit", default_limit))
    limit = min(max(limit, 0), max_limit)

    return offset, limit


def get_sort_filter(fields):
    """
    Parse flask.request to create clauses for SQLAlchemy's order_by and where.

    :param fields: A dictionary of field names to SQLAlchemy table columns
    listing what fields can be sorted/ordered on.
    :return: A 2-tuple of (where_clause, order_clause). order_clause is an
    ordered list of columns.
    """
    where_clause = True
    order_clause = []

    for filter_param in flask.request.args.getlist("filter"):
        field, cmp, value = filter_param.split(",")

        if field not in fields:
            raise util.APIError(
                400, message="Cannot filter on field {}".format(field))

        column = fields[field]
        conversion = None
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
                400, message="Cannot compare column by '{}'".format(cmp))

        clause = operation(column, value)
        if where_clause is True:
            where_clause = clause
        else:
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

    return where_clause, order_clause

######################
# USER API ENDPOINTS #
######################


@web_api.route("/user")
def list_users():
    result = []
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "user_id": model.users.c.userID,
        "username": model.users.c.username,
        "level": model.users.c.level,
        "rank": model.users.c.rank,
        "num_submissions": model.users.c.numSubmissions,
        "num_games": model.users.c.numGames,
        "organization_id": model.users.c.organizationID,
    })

    with model.engine.connect() as conn:
        query = conn.execute(
            model.users.select()
                .where(where_clause).order_by(*order_clause)
                .offset(offset).limit(limit)
        )

        for row in query.fetchall():
            user = {
                "user_id": row["userID"],
                "username": row["username"],
                "level": row["level"],
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "organization_id": row["organizationID"],
            }
            if row["countryVisible"] == 1:
                user["country_code"] = row["countryCode"]
                user["country_subdivision_code"] = row["countrySubdivisionCode"]

            result.append(user)

    return flask.jsonify(result)


@web_api.route("/user", methods=["POST"])
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
            model.users.c.userID == user_id
        )).first()

        if user_data["verificationCode"]:
            raise util.APIError(400, message="User needs to verify email.")

        if user_data["isEmailGood"] == 1:
            raise util.APIError(400, message="User already validated.")

    org_id = body.get("organization_id")
    email = body.get("email")
    level = body.get("level", model.users.c.level)
    verification_code = str(random.randint(0, 2**63))

    # Values to insert into the database
    values = {}

    # Perform validation on given values
    if "level" in body and not validate_user_level(body["level"]):
        raise util.APIError(400, message="Invalid user level.")

    if "country_code" in body or "country_subdivision_code" in body:
        country_code = body.get("country_code")
        subdivision_code = body.get("country_subdivision_code")

        if subdivision_code and not country_code:
            raise util.APIError(400, message="Must provide country code if country subdivision code is given.")

        if not validate_country(country_code, subdivision_code):
            raise util.APIError(400, message="Invalid country/country subdivision code.")

        values["countryCode"] = country_code
        values["countrySubdivisionCode"] = subdivision_code

    if "country_visible" in body:
        if body["country_visible"] not in ("0", "1"):
            raise util.APIError(400, message="Invalid country_visible (must be 0 or 1).")
        else:
            values["countryVisible"] = int(body["country_visible"])

    # Figure out the situation with their email/organization
    if org_id is None and email is None:
        # Just use their Github email. Don't bother guessing an affiliation
        # (we can do that client-side)
        values.update({
            "email": model.users.c.githubEmail,
            "isEmailGood": 1,
            "organizationID": None,
            "level": level,
        })
        message = "User all set."
    elif org_id is None:
        values.update({
            "email": email,
            "isEmailGood": 0,
            "verificationCode": verification_code,
            "organizationID": None,
            "level": level,
        })
        message = "User needs to validate email."
    else:
        # Check the org
        with model.engine.connect() as conn:
            org = conn.execute(model.organizations.select().where(
                model.organizations.c.organizationID == org_id
            )).first()

            if org is None:
                raise util.APIError(400, message="Organization does not exist.")

            # Verify the email against the org
            email_to_verify = email or user_data["githubEmail"]
            if "@" not in email_to_verify:
                raise util.APIError(
                    400, message="Email should at least have an at sign...")
            domain = email.split("@")[1].strip().lower()
            count = conn.execute(sqlalchemy.sql.select([
                sqlalchemy.sql.func.count()
            ]).select_from(model.organization_email_domains).where(
                (model.organization_email_domains.c.organizationID == org_id) &
                (model.organization_email_domains.c.domain == domain)
            )).first()[0]

            if count == 0:
                raise util.APIError(
                    400, message="Invalid email for organization.")

        # Set the verification code (if necessary), and update the user's
        # level to match the organization's level.
        if email:
            values.update({
                "email": email,
                "isEmailGood": 0,
                "verificationCode": verification_code,
                "organizationID": org_id,
                "level": org["level"],
            })
            # TODO: send the verification email
            message = "User added to organization, but needs to validate email."
        else:
            values.update({
                "email": model.users.c.githubEmail,
                "isEmailGood": 1,
                "organizationID": org_id,
                "level": org["level"],
            })
            message = "User added to organization."

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.userID == user_id
        ).values(**values))

    return response_success({
        "message": message,
    })


@web_api.route("/user/<int:intended_user>", methods=["GET"])
@optional_login
def get_user(intended_user, *, user_id):
    with model.engine.connect() as conn:
        query = model.users.select(
            model.users.c.userID == intended_user
        ).select_from(
            model.users.join(
                model.organizations,
                model.users.c.organizationID == model.organizations.c.organizationID)
        )

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        user = {
            "user_id": row["userID"],
            "username": row["username"],
            "level": row["level"],
            "organization_id": row["organizationID"],
            "organization": row["organizationName"],
            # In the future, these would be the stats of their best bot
            # Right now, though, each user has at most 1 bot
            "rank": row["rank"],
            "points": row["mu"] - 3 * row["sigma"],
            "num_submissions": row["numSubmissions"],
            "num_games": row["numGames"],
        }
        if row["countryVisible"] == 1 or intended_user == user_id:
            user["country_code"] = row["countryCode"]
            user["country_subdivision_code"] = row["countrySubdivisionCode"]

        return flask.jsonify(user)


@web_api.route("/user/<int:user_id>/verify", methods=["GET"])
def verify_user_email(user_id):
    verification_code = flask.request.args.get("verification_code")
    if not verification_code:
        raise util.APIError(400, message="Please provide verification code.")

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.users.c.verificationCode,
        ]).where(
            model.users.c.userID == user_id
        )

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        if row["verificationCode"] == verification_code:
            conn.execute(model.users.update().where(
                model.users.c.userID == user_id
            ).values(
                isEmailGood=1,
                verificationCode="",
            ))
            return response_success({
                "message": "Email verified."
            })

        raise util.APIError(400, message="Invalid verification code.")


@web_api.route("/user/<int:intended_user_id>", methods=["PUT"])
@requires_login
def update_user(intended_user_id, *, user_id):
    if user_id != intended_user_id:
        raise user_mismatch_error()

    fields = flask.request.get_json()
    columns = {
        "level": "level",
        "country_code": "countryCode",
        "country_subdivision_code": "countrySubdivisionCode",
        "country_visible": "countryVisible",
    }

    update = {}

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

        update[columns[key]] = fields[key]

    if "level" in update and not validate_user_level(update["level"]):
        raise util.APIError(400, message="Invalid user level.")

    if "countryCode" in update or "countrySubdivisionCode" in update:
        with model.engine.connect() as conn:
            user = conn.execute(model.users.select(model.users.c.userID == user_id)).first()
        country_code = update.get("countryCode", user["countryCode"])
        subdivision_code = update.get("countrySubdivisionCode", user["countrySubdivisionCode"])

        if not validate_country(country_code, subdivision_code):
            raise util.APIError(400, message="Invalid country/country subdivision code.")

    if "countryVisible" in update:
        if update["countryVisible"] not in ("0", "1"):
            raise util.APIError(400, message="Invalid country_visible (must be 0 or 1).")
        else:
            update["countryVisible"] = int(update["countryVisible"])

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.userID == user_id
        ).values(**update))

    return response_success()


@web_api.route("/user/<int:intended_user_id>", methods=["DELETE"])
@requires_login
def delete_user(intended_user_id, *, user_id):
    # TODO: what happens to their games?
    if user_id != intended_user_id:
        raise user_mismatch_error()

    with model.engine.connect() as conn:
        conn.execute(model.users.delete().where(
            model.users.c.userID == user_id))


# ---------------------- #
# USER BOT API ENDPOINTS #
# ---------------------- #

# Currently, there is no notion of a user having multiple distinct bots.
# However, in the API, we pretend this is the case as much as possible, since
# we may wish to support this eventually.


@web_api.route("/user/<int:user_id>/bot", methods=["GET"])
def list_user_bots(user_id):
    # TODO: parameter to get only IDs

    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().where(
            model.users.c.userID == user_id
        )).fetchall()

        if len(query) != 1:
            raise util.APIError(404, message="User not found.")

        row = query[0]

        if row["numSubmissions"] == 0:
            return flask.jsonify([])

        return flask.jsonify([
            {
                "bot_id": 0,
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "language": row["language"],
            }
        ])


@web_api.route("/user/<int:user_id>/bot/<int:bot_id>", methods=["GET"])
def get_user_bot(user_id, bot_id):
    with model.engine.connect() as conn:
        query = conn.execute(model.users.select().where(
            model.users.c.userID == user_id
        )).fetchall()

        if len(query) != 1:
            raise util.APIError(404, message="User not found.")

        if bot_id != 0:
            raise util.APIError(404, message="Bot not found.")

        row = query[0]

        if row["numSubmissions"] == 0:
            return flask.jsonify([])

        return flask.jsonify([
            {
                "bot_id": 0,
                "rank": row["rank"],
                "num_submissions": row["numSubmissions"],
                "num_games": row["numGames"],
                "language": row["language"],
            }
        ])

# TODO: POST to just /bot to create a new bot
@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["PUT"])
@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["POST"])
@requires_login
def store_user_bot(user_id, intended_user, bot_id):
    """Store an uploaded bot in object storage."""
    if not config.COMPETITION_OPEN:
        raise util.APIError(
            400, message="Sorry, but bot submissions are closed."
        )

    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot upload bot for another user.")

    conn = model.engine.connect()
    user = conn.execute(model.users.select(model.users.c.userID == user_id)) \
        .first()

    # Check if the user already has a bot compiling
    if user["compileStatus"] != 0:
        raise util.APIError(400, message="Cannot upload new bot until "
                                         "previous one is compiled.")

    if "botFile" not in flask.request.files:
        raise util.APIError(400, message="Bot file not provided (must "
                                         "provide as botFile).")

    # Save to GCloud
    uploaded_file = flask.request.files["botFile"]
    blob = gcloud_storage.Blob(str(user_id), model.get_compilation_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)

    # Flag the user as compiling
    update = model.users.update() \
        .where(model.users.c.userID == user_id) \
        .values(compileStatus=1)
    conn.execute(update)

    # TODO: Email the user

    # TODO: Spin up more workers?

    return response_success()


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["DELETE"])
@requires_login
def delete_user_bot(intended_user, bot_id, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot delete bot for another user.")

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.userID == user_id
        ).values(
            isRunning=0,
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
def reset_api_key(intended_user, *, user_id):
    if user_id != intended_user:
        raise user_mismatch_error(
            message="Cannot reset another user's API key.")

    chars = string.ascii_letters + string.digits
    with model.engine.connect() as conn:
        api_key = "".join(random.choice(chars) for _ in range(32))

        conn.execute(model.users.update().where(
            model.users.c.userID == user_id
        ).values(
            apiKeyHash=config.api_key_context.hash(api_key),
        ))

        return response_success({
            "api_key": "{}:{}".format(user_id, api_key),
        })


# ------------------------ #
# USER MATCH API ENDPOINTS #
# ------------------------ #
@web_api.route("/user/<int:intended_user>/match", methods=["GET"])
def list_user_matches(intended_user):
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "game_id": model.games.c.gameID,
        "timestamp": model.games.c.timestamp,
    })
    result = []

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.games.c.gameID,
            model.games.c.replayName,
            model.games.c.mapWidth,
            model.games.c.mapHeight,
            model.games.c.timestamp,
        ]).select_from(model.games.join(
            model.gameusers,
            (model.games.c.gameID == model.gameusers.c.gameID) &
            (model.gameusers.c.userID == intended_user),
        )).where(where_clause).order_by(*order_clause).offset(offset).limit(limit)
        matches = conn.execute(query)

        for match in matches.fetchall():
            result.append({
                "game_id": match["gameID"],
                "map_width": match["mapWidth"],
                "map_height": match["mapHeight"],
                "replay": match["replayName"],
                "timestamp": match["timestamp"],
            })

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>", methods=["GET"])
@optional_login
def get_user_match(intended_user, match_id, *, user_id):
    with model.engine.connect() as conn:
        query = conn.execute(sqlalchemy.sql.select([
            model.gameusers.c.userID,
            model.gameusers.c.rank,
            model.gameusers.c.playerIndex,
            model.gameusers.c.didTimeout,
            model.gameusers.c.errorLogName,
        ]).where(
            model.gameusers.c.gameID == match_id
        ))

        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replayName,
            model.games.c.mapWidth,
            model.games.c.mapHeight,
            model.games.c.timestamp,
        ]).where(
            model.games.c.gameID == match_id
        )).first()

        result = {
            "map_width": match["mapWidth"],
            "map_height": match["mapHeight"],
            "replay": match["replayName"],
            "timestamp": match["timestamp"],
            "players": {}
        }
        for row in query.fetchall():
            result["game_id"] = match_id
            result["players"][row["userID"]] = {
                "rank": row["rank"],
                "player_index": row["playerIndex"],
                "timed_out": bool(row["didTimeout"]),
            }

            if (user_id is not None and intended_user == user_id and
                    row["userID"] == user_id):
                result["players"][row["userID"]]["error_log"] = row["errorLogName"]

    return flask.jsonify(result)


@web_api.route("/user/<int:intended_user>/match/<int:match_id>/replay",
               methods=["GET"])
def get_match_replay(intended_user, match_id):
    with model.engine.connect() as conn:
        match = conn.execute(sqlalchemy.sql.select([
            model.games.c.replayName,
        ]).where(
            model.games.c.gameID == match_id
        )).first()

        blob = gcloud_storage.Blob(match["replayName"],
                                   model.get_replay_bucket(),
                                   chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(buffer, mimetype="application/x-halite-2-replay",
                               as_attachment=True,
                               attachment_filename=str(match_id)+".hlt")


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
            model.gameusers.c.errorLogName,
        ]).where(
            (model.gameusers.c.gameID == match_id) &
            (model.gameusers.c.userID == user_id)
        )).first()

        if match is None:
            raise util.APIError(
                404, message="Game does not exist."
            )

        if match["errorLogName"] is None:
            raise util.APIError(
                404, message="No error log for this player in this game."
            )

        blob = gcloud_storage.Blob(match["errorLogName"],
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
def list_organizations():
    result = []
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "organization_id": model.organizations.c.organizationID,
        "organization_name": model.organizations.c.organizationName,
        "level": model.organizations.c.level,
    })

    with model.engine.connect() as conn:
        query = model.organizations.select()\
            .where(where_clause).order_by(*order_clause)\
            .offset(offset).limit(limit)
        orgs = conn.execute(query)

        for org in orgs.fetchall():
            result.append({
                "organization_id": org["organizationID"],
                "name": org["organizationName"],
                "level": org["level"],
            })

    return flask.jsonify(result)


@web_api.route("/organization/<int:org_id>", methods=["GET"])
def get_organization(org_id):
    with model.engine.connect() as conn:
        org = conn.execute(model.organizations.select().where(
            model.organizations.c.organizationID == org_id
        )).first()

        if not org:
            raise util.APIError(404, message="Organization not found.")

        return flask.jsonify({
            "organization_id": org["organizationID"],
            "name": org["organizationName"],
            "level": org["level"],
        })


@web_api.route("/organization", methods=["POST"])
@requires_admin
def create_organization():
    org_body = flask.request.get_json()
    if "name" not in org_body:
        raise util.APIError(400, message="Organization must be named.")

    with model.engine.connect() as conn:
        org_id = conn.execute(model.organizations.insert().values(
            organizationName=org_body["name"],
            level=org_body.get("level", "Professional"),
        )).inserted_primary_key

    return response_success({
        "organization_id": org_id[0],
    })


@web_api.route("/organization/<int:org_id>", methods=["PUT"])
@requires_admin
def update_organization(org_id):
    fields = flask.request.get_json()
    columns = {
        "name": model.organizations.c.organizationName,
    }

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

    with model.engine.connect() as conn:
        conn.execute(model.organizations.update().where(
            model.organizations.c.organizationID == org_id
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
                model.users.c.organizationID == org_id
            )).first()[0]

            if count > 0:
                raise util.APIError(
                    400, message="Cannot delete organization with members.")

            conn.execute(model.organizations.delete().where(
                model.organizations.c.organizationID == org_id
            ))

    return response_success()


#############################
# LEADERBOARD API ENDPOINTS #
#############################
@web_api.route("/leaderboard")
def leaderboard():
    offset, limit = get_offset_limit()
    where_clause, order_clause = get_sort_filter({
        "user_id": model.users.c.userID,
        "username": model.users.c.username,
        "level": model.users.c.level,
        "organization_id": model.users.c.organizationID,
        "language": model.users.c.language,
        "points": model.users.c.mu,
        "rank": model.users.c.rank,
    })
    if not order_clause:
        order_clause = [model.users.c.rank.asc()]
    result = []

    with model.engine.connect() as conn:
        query = sqlalchemy.sql.select([
            model.users.c.userID,
            model.users.c.username,
            model.users.c.level,
            model.users.c.organizationID,
            model.organizations.c.organizationName,
            model.users.c.language,
            model.users.c.mu,
            model.users.c.rank,
        ]).select_from(
            model.users.join(
                model.organizations,
                model.users.c.organizationID == model.organizations.c.organizationID)
        ).where(where_clause).order_by(*order_clause).offset(offset).limit(limit)
        players = conn.execute(query)

        for player in players.fetchall():
            result.append({
                "user_id": player["userID"],
                "username": player["username"],
                "level": player["level"],
                "organization_id": player["organizationID"],
                "organization": player["organizationName"],
                "language": player["language"],
                "points": player["mu"],
                "rank": player["rank"],
            })

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
            model.users.c.userID == user_id,
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
    if user["isEmailGood"] != 1:
        raw_payload["require_activation"] = "true"

    encoded_payload = base64.b64encode(
        urllib.parse.urlencode(raw_payload).encode("utf-8")).decode("utf-8")
    new_signature = hmac.new(config.DISCOURSE_SSO_SECRET,
                             msg=encoded_payload,
                             digestmod=hashlib.sha256).hexdigest()
    return flask.redirect(
        config.DISCOURSE_URL +
        "?sso={}&sig={}".format(encoded_payload, new_signature))

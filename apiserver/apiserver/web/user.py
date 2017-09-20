"""
User API endpoints - create/update/delete/list users and user data
"""

import uuid

import flask
import sqlalchemy

from .. import config, model, notify, util

from . import util as web_util
from .blueprint import web_api

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


def verify_affiliation(org_id, email_to_verify, provided_code):
    """
    Verify whether a user is allowed to associate with an organization.

    :param org_id: The ID of the organization in question.
    :param email_to_verify: The email of the user.
    :param provided_code: A verification code for the org, if provided.
    :return: Nothing (raises util.APIError if user cannot affiliate)
    """
    with model.engine.connect() as conn:
        org = conn.execute(model.organizations.select().where(
            model.organizations.c.id == org_id
        )).first()

        if org is None:
            raise util.APIError(404, message="Organization does not exist.")

        # Verify the email against the org
        if "@" not in email_to_verify:
            raise util.APIError(400, message="Email invalid.")
        domain = email_to_verify.split("@")[1].strip().lower()
        count = conn.execute(sqlalchemy.sql.select([
            sqlalchemy.sql.func.count()
        ]).select_from(model.organization_email_domains).where(
            (model.organization_email_domains.c.organization_id == org_id) &
            (model.organization_email_domains.c.domain == domain)
        )).first()[0]

        can_verify_by_code = org["verification_code"] is not None
        email_error = util.APIError(
            400, message="Invalid email for organization.")
        verification_error = util.APIError(
            400, message="Invalid verification code.")
        code_correct = org["verification_code"] == provided_code

        if count == 0:
            if can_verify_by_code and not code_correct:
                raise verification_error
            else:
                raise email_error
        elif can_verify_by_code:
            if not code_correct:
                raise verification_error

    # Otherwise, no verification method defined, or passed verification


def send_verification_email(recipient, verification_code):
    """
    Send the verification email to the user.

    :param notify.Recipient recipient:
    :param verification_code:
    :return:
    """

    notify.send_templated_notification(
        recipient,
        config.VERIFY_EMAIL_TEMPLATE,
        {
            "verification_url": util.build_site_url("/verify-email", {
                "verification_code": verification_code,
            }),
        },
        config.GOODNEWS_ACCOMPLISHMENTS,
        config.C_EMAIL_VERIFICATION
    )


def send_confirmation_email(recipient):
    """
    Send a confirmation email to the user (let them know that they registered)

    :param notify.Recipient recipient:
    """

    notify.send_templated_notification(
        recipient,
        config.CONFIRMATION_TEMPLATE,
        {},
        config.GOODNEWS_ACCOMPLISHMENTS,
        config.C_REGISTRATION_CONFIRMATION
    )


@web_api.route("/user")
@util.cross_origin(methods=["GET", "POST"])
def list_users():
    result = []
    offset, limit = web_util.get_offset_limit()

    where_clause, order_clause, _ = web_util.get_sort_filter({
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
        total_users = conn.execute(model.total_ranked_users).first()[0]

        query = conn.execute(
            model.all_users.select()
                    .where(where_clause).order_by(*order_clause)
                    .offset(offset).limit(limit).reduce_columns())

        for row in query.fetchall():
            result.append(make_user_record(row, logged_in=False,
                                           total_users=total_users))

    return flask.jsonify(result)


@web_api.route("/user", methods=["POST"])
@util.cross_origin(methods=["GET", "POST"])
@web_util.requires_login(accept_key=False)
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
    level = body.get("level", user_data["player_level"])
    provided_code = body.get("verification_code", None)
    verification_code = uuid.uuid4().hex

    # Values to insert into the database
    values = {}

    # Perform validation on given values
    if "level" in body and not web_util.validate_user_level(body["level"]):
        raise util.APIError(400, message="Invalid user level.")

    if email is not None and "@" not in email:
        raise util.APIError(400, message="Invalid user email.")

    if "country_code" in body or "country_subdivision_code" in body:
        country_code = body.get("country_code")
        subdivision_code = body.get("country_subdivision_code")

        if subdivision_code and not country_code:
            raise util.APIError(
                400,
                message="Must provide country code if country subdivision code is given.")

        if not web_util.validate_country(country_code, subdivision_code):
            raise util.APIError(
                400, message="Invalid country/country subdivision code.")

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
        verify_affiliation(org_id, email or user_data["github_email"],
                           provided_code)

    # Set the email verification code (if necessary).
    if email:
        values.update({
            "email": email,
            "is_email_good": 0,
            "verification_code": verification_code,
            "organization_id": org_id,
        })

        organization_name = None
        if org_id:
            with model.engine.connect() as conn:
                organization_data = conn.execute(model.organizations.select(
                    model.organizations.c.id == org_id
                )).first()
                if organization_data:
                    organization_name = organization_data["organization_name"]

        send_verification_email(
            notify.Recipient(user_id, user_data["username"], email,
                             organization_name, level,
                             user_data["creation_time"]),
            verification_code)

        message = "Please check your email for a verification code."
    else:
        values.update({
            "email": model.users.c.github_email,
            "is_email_good": 1,
            "organization_id": org_id,
        })
        message = "You've been added to the organization automatically!"

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(**values))

    return util.response_success({
        "message": message,
    }, status_code=201)


@web_api.route("/user/<int:intended_user>", methods=["GET"])
@util.cross_origin(methods=["GET"])
@web_util.requires_login(optional=True, accept_key=True)
def get_user(intended_user, *, user_id):
    with model.engine.connect() as conn:
        query = model.all_users.select(
            model.all_users.c.user_id == intended_user)

        row = conn.execute(query).first()
        if not row:
            raise util.APIError(404, message="No user found.")

        total_users = conn.execute(model.total_ranked_users).first()[0]

        logged_in = user_id is not None and intended_user == user_id
        user = make_user_record(row, logged_in=logged_in,
                                total_users=total_users)

        return flask.jsonify(user)


@web_api.route("/user/<int:user_id>/verify", methods=["POST"])
@util.cross_origin(methods=["POST"])
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
            return util.response_success({
                "message": "Email verified."
            })
        elif row["is_email_good"]:
            return util.response_success({
                "message": "Email already verified.",
            })

        raise util.APIError(400, message="Invalid verification code.")


@web_api.route("/user/<int:intended_user_id>", methods=["PUT"])
@web_util.requires_login(accept_key=False)
def update_user(intended_user_id, *, user_id):
    if user_id != intended_user_id:
        raise web_util.user_mismatch_error()

    fields = flask.request.get_json()
    columns = {
        "level": "player_level",
        "country_code": "country_code",
        "country_subdivision_code": "country_subdivision_code",
        "organization_id": "organization_id",
        "email": "email",
        "verification_code": "organization_verification_code",
    }

    update = {}

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

        update[columns[key]] = fields[key]

    # Validate new player level
    if ("player_level" in update and
            not web_util.validate_user_level(update["player_level"])):
        raise util.APIError(400, message="Invalid player level.")

    # Validate new country/region, if provided
    if "country_code" in update or "country_subdivision_code" in update:
        with model.engine.connect() as conn:
            user = conn.execute(
                model.users.select(model.users.c.id == user_id)).first()
        country_code = update.get("country_code", user["country_code"])
        subdivision_code = update.get("country_subdivision_code",
                                      user["country_subdivision_code"])

        if not web_util.validate_country(country_code, subdivision_code):
            raise util.APIError(
                400, message="Invalid country/country subdivision code.")

    if "organization_id" in update:
        # Associate the user with the organization
        if "email" not in update:
            raise util.APIError(
                400, message="Provide email to associate with organization."
            )
        verify_affiliation(update["organization_id"], update["email"],
                           update.get("organization_verification_code"))

    if "organization_verification_code" in update:
        del update["organization_verification_code"]

    if "email" in update:
        update["verification_code"] = uuid.uuid4().hex

    with model.engine.connect() as conn:
        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(**update))

        user_data = conn.execute(model.users.select(
            model.users.c.id == intended_user_id).join(
            model.organizations,
            model.users.c.organization_id == model.organizations.c.id,
            isouter=True
        )).first()

        if "email" in update:
            send_verification_email(
                notify.Recipient(user_id, user_data["username"],
                                 user_data["email"],
                                 user_data["organization_name"],
                                 user_data["player_level"],
                                 user_data["creation_time"]),
                update["verification_code"])
        else:
            send_confirmation_email(
                notify.Recipient(user_id, user_data["username"],
                                 user_data["email"],
                                 user_data["organization_name"],
                                 user_data["player_level"],
                                 user_data["creation_time"]))

    return util.response_success()


@web_api.route("/user/<int:intended_user_id>", methods=["DELETE"])
@web_util.requires_login(accept_key=True, admin=True)
def delete_user(intended_user_id, *, admin_id):
    # TODO: what happens to their games?
    with model.engine.connect() as conn:
        conn.execute(model.users.delete().where(
            model.users.c.id == intended_user_id))

@web_api.route("/user/addsubscriber/<string:recipient>", methods=["POST"])
@util.cross_origin(methods=["POST"])
def add_subscriber(recipient):
    notify.add_user_to_contact_list(recipient)
    notify.send_templated_notification_simple(
        recipient,
        config.NEW_SUBSCRIBER_TEMPLATE,
        config.GOODNEWS_ACCOMPLISHMENTS,
        config.C_NEWSLETTER_SUBSCRIPTION)
    return util.response_success()

@web_api.route("/api_key", methods=["POST"])
@web_api.route("/user/<int:intended_user>/api_key", methods=["POST"])
@util.cross_origin(methods=["POST"])
@web_util.requires_login(accept_key=False, association=True)
def reset_api_key(intended_user=None, *, user_id):
    if user_id != intended_user and intended_user is not None:
        raise web_util.user_mismatch_error(
            message="Cannot reset another user's API key.")

    with model.engine.connect() as conn:
        api_key = uuid.uuid4().hex

        conn.execute(model.users.update().where(
            model.users.c.id == user_id
        ).values(
            api_key_hash=config.api_key_context.hash(api_key),
        ))

        return util.response_success({
            "api_key": "{}:{}".format(user_id, api_key),
        }, status_code=201)

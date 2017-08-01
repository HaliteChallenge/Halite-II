"""
User bot API endpoints - create/update/delete/list user's bots
"""
import zipfile

import flask
import sqlalchemy

import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions

from .. import config, model, util
from .. import response_success
from ..util import cross_origin

from .util import user_mismatch_error, requires_login, requires_association, \
    requires_competition_open, requires_admin
from .blueprint import web_api


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
@requires_competition_open
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
            compile_status=model.CompileStatus.DISABLED.value,
        ))

    store_user_bot(intended_user=intended_user, user_id=user_id, bot_id=0)
    return response_success({
        "bot_id": 0,
    })


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["PUT"])
@cross_origin(methods=["GET", "PUT"])
@requires_login
@requires_association
@requires_competition_open
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
        if bot["compile_status"] == model.CompileStatus.IN_PROGRESS.value:
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
            compile_status=model.CompileStatus.UPLOADED.value,
            update_time=sqlalchemy.sql.func.now(),
        )
        conn.execute(update)

    # TODO: Email the user

    return response_success()


@web_api.route("/user/<int:intended_user>/bot/<int:bot_id>", methods=["DELETE"])
@requires_admin()
@requires_competition_open
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
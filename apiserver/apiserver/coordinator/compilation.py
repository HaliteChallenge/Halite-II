import datetime

import flask
import sqlalchemy

from .. import config, model, notify, util

from .blueprint import coordinator_api


def reset_compilation_tasks(conn):
    """Check ongoing compilation tasks, and reset ones that are "stuck"."""
    reset_stuck_tasks = model.bots.update().where(
        (model.bots.c.compile_status == model.CompileStatus.IN_PROGRESS.value) &
        (model.bots.c.compile_start <
         datetime.datetime.now() - datetime.timedelta(
             minutes=config.COMPILATION_STUCK_THRESHOLD))
    ).values(
        compile_status=model.CompileStatus.UPLOADED.value,
        compile_start=None,
    )
    conn.execute(reset_stuck_tasks)


def serve_compilation_task(conn):
    """Try to find and return a compilation task."""
    with conn.begin() as transaction:
        # Try to assign a compilation task.
        find_compilation_task = model.bots.select() \
            .where(model.bots.c.compile_status ==
                   model.CompileStatus.UPLOADED.value) \
            .with_for_update() \
            .order_by(model.bots.c.user_id.asc()) \
            .limit(1)
        bot = conn.execute(find_compilation_task).first()
        if bot:
            user_id = bot["user_id"]
            bot_id = bot["id"]
            # Keep track of when compilation started, so that we can
            # restart it if it gets stuck
            update = model.bots.update() \
                .where((model.bots.c.user_id == user_id) &
                       (model.bots.c.id == bot_id)) \
                .values(compile_status=model.CompileStatus.IN_PROGRESS.value,
                        compile_start=sqlalchemy.sql.func.now())
            conn.execute(update)
            return util.response_success({
                "type": "compile",
                "user": user_id,
                "bot": bot_id,
            })
    return None


# these aren't RESTful URLs, but it's what the worker expects
@coordinator_api.route("/compile", methods=["POST"])
def update_compilation_status():
    """Update the compilation status of a bot."""
    user_id = flask.request.form.get("user_id", None)
    bot_id = flask.request.form.get("bot_id", None)
    did_compile = bool(int(flask.request.form.get("did_compile", '0')))
    language = flask.request.form.get("language", "Other")
    errors = flask.request.form.get("errors", "")

    if user_id is None:
        raise util.APIError(400, message="Must provide user ID.")

    with model.engine.begin() as conn:
        user = conn.execute(sqlalchemy.sql.select([
            model.users.c.id,
            model.users.c.username,
            model.users.c.github_email.label("email"),
            model.users.c.github_email,
            model.users.c.is_active,
            model.users.c.on_email_list,
            model.users.c.is_email_good,
            model.users.c.player_level,
            model.users.c.country_code,
            model.users.c.country_subdivision_code,
            model.users.c.creation_time,
            model.users.c.update_time,
            model.organizations.c.organization_name,
        ]).select_from(model.users.join(
            model.organizations,
            model.users.c.organization_id == model.organizations.c.id,
            isouter=True)
        ).where(
            model.users.c.id == user_id
        )).first()
        bot = conn.execute(model.bots.select(
            (model.bots.c.user_id == user_id) &
            (model.bots.c.id == bot_id)
        )).first()

        bot_rank = conn.execute(sqlalchemy.sql.select([
            sqlalchemy.sql.text("ranked_bots.bot_rank")
        ]).where(
            (model.ranked_bots.c.user_id == user_id) &
            (model.ranked_bots.c.bot_id == bot_id)
        )).first()

        if not user:
            raise util.APIError(400, message="User not found.")
        if not bot:
            raise util.APIError(400, message="Bot not found.")

        update = model.bots.update() \
            .where((model.bots.c.user_id == user_id) &
                   (model.bots.c.id == bot_id)) \
            .values(
            compile_status=(model.CompileStatus.SUCCESSFUL.value
                            if did_compile
                            else model.CompileStatus.FAILED.value),
            compile_start=None,
        )
        conn.execute(update)

        if did_compile:
            # This is backwards of the order in the original PHP, but the
            # original PHP updated the table using the -old- values of the
            # User row. This ordering makes it clearer that this is
            # intentional.

            # If version number is 0, then this is the first time the bot
            # has ever been submitted, so there's nothing to put in the
            # history table.
            if bot["version_number"] != 0:
                num_active_users = conn.execute(
                    sqlalchemy.sql.select([
                        sqlalchemy.sql.func.count()
                    ]).select_from(model.users)
                ).first()[0]

                try:
                    conn.execute(
                        model.bot_history.insert().values(
                            user_id=user_id,
                            bot_id=bot_id,
                            version_number=bot["version_number"],
                            # User is unranked if this is their first bot
                        last_rank=bot_rank[0] if bot_rank else None,
                            last_score=bot["score"],
                            last_num_players=num_active_users,
                            last_games_played=bot["games_played"],
                            language=bot["language"],
                        )
                    )
                except sqlalchemy.exc.IntegrityError:
                    # If this happens, then we're trying to insert a
                    # row with a duplicate version number - something
                    # previously prevented us from incrementing
                    # it. Instead of dying here, silence the exception
                    # so that we can increment the version number now.
                    pass

            # We do not reset the bot rank for new submissions now.
            conn.execute(
                model.bots.update().where(
                    (model.bots.c.user_id == user_id) &
                    (model.bots.c.id == bot_id)
                ).values(
                    language=language,
                    version_number=model.bots.c.version_number + 1,
                    games_played=0,
                    sigma=8.333,
                )
            )

            bot = conn.execute(model.bots.select().where(
                (model.bots.c.user_id == user_id) &
                (model.bots.c.id == bot_id)
            )).first()

            notify.send_templated_notification(
                notify.Recipient(user["id"], user["username"], user["email"],
                                 user["organization_name"], user["player_level"],
                                 user["creation_time"]),
                config.COMPILATION_SUCCESS_TEMPLATE,
                {
                    "version_number": bot["version_number"],
                    "detected_language": language,
                },
                config.GOODNEWS_ACCOMPLISHMENTS,
                config.C_COMPLIATION_SUCCESS
            )

            return util.response_success()
        else:
            notify.send_templated_notification(
                notify.Recipient(user["id"], user["username"], user["email"],
                                 user["organization_name"], user["player_level"],
                                 user["creation_time"]),
                config.COMPILATION_FAILURE_TEMPLATE,
                {
                    "version_number": bot["version_number"],
                    "detected_language": language,
                    "errors": errors,
                },
                config.GAME_ERROR_MESSAGES,
                config.C_COMPILATION_ERROR
            )
            return util.response_success()

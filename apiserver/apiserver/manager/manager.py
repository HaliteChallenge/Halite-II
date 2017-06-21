import flask
import sqlalchemy

import google.cloud.storage as gcloud_storage

from .. import model, response_success, response_failure


manager_api = flask.Blueprint("manager_api", __name__)


@manager_api.route("/task")
def task():
    """Serve compilation and game tasks to worker instances."""
    conn = model.engine.connect()
    find_compilation_task = model.users\
        .select(model.users.c.userID)\
        .where(model.users.c.compileStatus == 1)\
        .order_by(sqlalchemy.asc(model.users.c.userID))\
        .limit(1)
    users = conn.execute(find_compilation_task).fetchall()
    if users:
        user_id = users[0]["userID"]
        # TODO: some way to clear compileStatus if it gets stuck
        update = model.users.update() \
            .where(model.users.c.userID == user_id) \
            .values(compileStatus=2)
        conn.execute(update)
        return response_success({
            "type": "compile",
            "user": user_id,
        })

    # TODO: implement task to run a game

    return response_success({
        "type": "notask",
    })


# TODO: these aren't RESTful URLs, but it's what the worker expects
@manager_api.route("/compile", methods=["POST"])
def update_compilation_status():
    """Update the compilation status of a bot."""
    user_id = flask.request.form.get("userID", None)
    did_compile = flask.request.form.get("didCompile", False)

    if user_id is None:
        return response_failure("Must provide user ID.")

    conn = model.engine.connect()

    language = flask.request.form.get("language", "Other")

    if did_compile:
        # TODO: email the user

        user = model.users.select().where(model.users.c.userID == user_id)
        user = conn.execute(user).first()

        # This is backwards of the order in the original PHP, but the original
        # PHP updated the table using the -old- values of the User row. This
        # ordering makes it clearer that this is intentional.
        if user["numSubmissions"] != 0:
            num_active_users = conn.execute(
                model.users\
                    .select([sqlalchemy.func.count(model.users.c.userID)])\
                    .where(model.users.c.isRunning == 1)
            ).first()[0]

            conn.execute(
                model.user_history.insert().values(
                    userID=user_id,
                    versionNumber=user["numSubmissions"],
                    lastRank=user["rank"],
                    lastNumPlayers=num_active_users,
                    lastNumGames=user["numGames"],
                )
            )

        update = model.users.update()\
            .where(model.users.c.userID == user_id)\
            .values(
                numSubmissions=model.users.c.numSubmissions + 1,
                numGames=0,
                mu=25.000,
                sigma=8.333,
                compileStatus=0,
                isRunning=1,
                language=language,
            )
        return response_success()
    else:
        # TODO: email the user

        update = model.users.update()\
            .where(model.users.c.userID == user_id)\
            .values(compileStatus=0)
        conn.execute(update)
        return response_success()


@manager_api.route("/botFile", methods=["POST"])
def upload_bot():
    user_id = flask.request.form.get("userID", None)

    if "bot.zip" not in flask.request.files:
        return response_failure("Please provide the bot file.")

    uploaded_file = flask.request.files["bot.zip"]
    # Save to GCloud
    blob = gcloud_storage.Blob(str(user_id), model.get_bot_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)
    return response_success()


@manager_api.route("/botFile", methods=["GET"])
def download_bot():
    # TODO: implement bot downloading
    pass


@manager_api.route("/botHash")
def hash_bot():
    """Get the MD5 hash of a compiled bot."""
    user_id = flask.request.args.get("userID", None)
    compile = flask.request.args.get("compile", False)

    if not user_id:
        return response_failure("Please provide the user ID.")

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    blob = bucket.get_blob(str(user_id))
    if blob is None:
        return response_failure("Bot does not exist.")

    return blob.md5_hash


@manager_api.route("/games", methods=["POST"])
def upload_game():
    pass
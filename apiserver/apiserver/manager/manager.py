import base64
import binascii
import functools
import io

import flask
import sqlalchemy

import google.cloud.storage as gcloud_storage

from .. import model, response_success, response_failure


manager_api = flask.Blueprint("manager_api", __name__)


def requires_valid_worker(view):
    """
    Decorator that checks that the remote client is a valid worker.

    A valid worker must provide an API key.

    API keys are generated and stored when the worker is initially started.
    """
    @functools.wraps(view)
    def _requires_valid_worker(*args, **kwargs):
        api_key = flask.request.values.get("apiKey", None)
        if api_key is None:
            return flask.abort(401)

        conn = model.engine.connect()
        find_worker = model.workers\
            .select(model.workers.c.ipAddress)\
            .where(model.workers.c.apiKey == api_key)
        if len(conn.execute(find_worker).fetchall()) != 1:
            return flask.abort(401)

        kwargs["api_key"] = api_key
        return view(*args, **kwargs)

    return _requires_valid_worker


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

    # Increment the number of compilation tasks this worker has completed
    # TODO: this field is never actually used
    # TODO: we need the API key
    # $this->insert("UPDATE Worker SET numCompiles=numCompiles+1 WHERE apiKey=".$this->mysqli->real_escape_string($this->apiKey));

    update = model.users.update()\
        .where(model.users.c.userID == user_id)\
        .values(compileStatus=0)
    conn.execute(update)

    if did_compile:
        # TODO: email the user

        user = model.users.select().where(model.users.c.userID == user_id)
        user = conn.execute(user).first()

        # This is backwards of the order in the original PHP, but the original
        # PHP updated the table using the -old- values of the User row. This
        # ordering makes it clearer that this is intentional.
        if user["numSubmissions"] != 0:
            # TODO: make this more efficient
            num_active_users = len(conn.execute(
                model.users
                    .select(model.users.c.userID)
                    .where(model.users.c.isRunning == 1)
            ).fetchall())

            conn.execute(
                model.user_history.insert().values(
                    userID=user_id,
                    versionNumber=user["numSubmissions"],
                    # TODO: figure out why this isn't defined
                    lastRank=user["rank"] or 0,
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
        conn.execute(update)
        return response_success()
    else:
        # TODO: email the user

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
    user_id = flask.request.values.get("userID", None)
    compile = flask.request.values.get("compile", False)

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    # Retrieve from GCloud
    blob = gcloud_storage.Blob(str(user_id), bucket, chunk_size=262144)
    buffer = io.BytesIO()
    blob.download_to_file(buffer)
    buffer.seek(0)
    return flask.send_file(buffer, mimetype="application/zip",
                           as_attachment=True,
                           attachment_filename=str(user_id)+".zip")


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

    return response_success({
        "hash": binascii.hexlify(base64.b64decode(blob.md5_hash)).decode('utf-8'),
    })


@manager_api.route("/games", methods=["POST"])
def upload_game():
    pass
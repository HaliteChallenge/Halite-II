import base64
import binascii
import io
import tempfile

import flask
import google.cloud.storage as gcloud_storage
import google.cloud.exceptions as gcloud_exceptions

from werkzeug.contrib.cache import FileSystemCache

from .. import config, model, util

from .blueprint import coordinator_api


# Cache the worker blob to avoid repeated requests to object storage
cache_dir = tempfile.TemporaryDirectory()
cache = FileSystemCache(cache_dir.name, default_timeout=60*5)


@coordinator_api.route("/download/worker", methods=["GET"])
def download_source_blob():
    """Retrieve the worker blob from object storage."""

    cached_blob = cache.get(config.WORKER_ARTIFACT_KEY)
    if cached_blob is None:
        print("Getting from GCloud", config.WORKER_ARTIFACT_KEY)
        # Retrieve from GCloud
        try:
            gcloud_blob = gcloud_storage.Blob(
                config.WORKER_ARTIFACT_KEY,
                model.get_deployed_artifacts_bucket(),
                chunk_size=262144)
            cached_blob = gcloud_blob.download_as_string()
            cache.set(config.WORKER_ARTIFACT_KEY, cached_blob)
        except gcloud_exceptions.NotFound:
            raise util.APIError(404, message="Worker blob not found.")

    if cached_blob is None:
        raise util.APIError(404, message="Worker blob not found.")

    print("Building buffer")
    buffer = io.BytesIO()
    buffer.write(cached_blob)
    buffer.seek(0)
    return flask.send_file(buffer, mimetype="application/gzip",
                           as_attachment=True,
                           attachment_filename="Halite.tgz")


@coordinator_api.route("/botFile", methods=["POST"])
def upload_bot():
    """Save a compiled bot to object storage."""
    user_id = flask.request.form.get("user_id", None)
    bot_id = flask.request.form.get("bot_id", None)

    if "bot.zip" not in flask.request.files:
        raise util.APIError(400, message="Please provide the bot file.")

    uploaded_file = flask.request.files["bot.zip"]
    # Save to GCloud
    blob = gcloud_storage.Blob("{}_{}".format(user_id, bot_id),
                               model.get_bot_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)
    return util.response_success()


@coordinator_api.route("/botFile", methods=["GET"])
def download_bot():
    """Retrieve a compiled or uncompiled bot from object storage."""
    user_id = flask.request.values.get("user_id", None)
    bot_id = flask.request.values.get("bot_id", None)
    compile = flask.request.values.get("compile", False)

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    # Retrieve from GCloud
    try:
        botname = "{}_{}".format(user_id, bot_id)
        blob = gcloud_storage.Blob(botname,
                                   bucket, chunk_size=262144)
        buffer = io.BytesIO()
        blob.download_to_file(buffer)
        buffer.seek(0)
        return flask.send_file(buffer, mimetype="application/zip",
                               as_attachment=True,
                               attachment_filename=botname + ".zip")
    except gcloud_exceptions.NotFound:
        raise util.APIError(404, message="Bot not found.")


@coordinator_api.route("/botHash")
def hash_bot():
    """Get the MD5 hash of a compiled bot."""
    user_id = flask.request.args.get("user_id", None)
    bot_id = flask.request.args.get("bot_id", None)
    compile = flask.request.args.get("compile", False)

    if not user_id or not bot_id:
        raise util.APIError(400, message="Please provide user and bot ID.")

    if compile:
        bucket = model.get_compilation_bucket()
    else:
        bucket = model.get_bot_bucket()

    blob = bucket.get_blob("{}_{}".format(user_id, bot_id))
    if blob is None:
        raise util.APIError(400, message="Bot does not exist.")

    return util.response_success({
        "hash": binascii.hexlify(base64.b64decode(blob.md5_hash)).decode('utf-8'),
    })
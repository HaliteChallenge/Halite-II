import os.path

import flask
import google.cloud.storage as gcloud_storage
import sqlalchemy

from .. import config, model
from .. import requires_login_api, response_failure, response_success


web_api = flask.Blueprint("web_api", __name__)


@web_api.route("/botFile", methods=["POST"])
@requires_login_api
def upload_bot(*, user_id):
    """Store an uploaded bot in object storage."""
    if not config.COMPETITION_OPEN:
        return response_failure("Sorry, but bot submissions are closed.")

    conn = model.engine.connect()
    user = conn.execute(model.users.select(model.users.c.userID == user_id))\
        .first()

    # Check if the user already has a bot compiling
    if user["compileStatus"] != 0:
        return response_failure("Please wait for the current bot to finish "
                                "compiling.")

    if "botFile" not in flask.request.files:
        return response_failure("Please provide the bot file.")

    uploaded_file = flask.request.files["botFile"]
    if uploaded_file.filename == "" or \
            not allowed_file(uploaded_file.filename):
        return response_failure("Please provide the bot file with a valid "
                                "name. The file must be a .zip file.")

    # Save to GCloud
    blob = gcloud_storage.Blob(str(user_id), model.get_compilation_bucket(),
                               chunk_size=262144)
    blob.upload_from_file(uploaded_file)

    # Flag the user as compiling
    update = model.users.update()\
        .where(model.users.c.userID == user_id)\
        .values(compileStatus=1)
    conn.execute(update)

    # TODO: Email the user

    # TODO: Spin up more workers?

    return flask.jsonify({
        "status": "success",
    })


def allowed_file(filename):
    _, extension = os.path.splitext(filename)
    return extension == ".zip"

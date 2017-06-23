import os.path

import flask
import google.cloud.storage as gcloud_storage

from .. import config, model
from .. import requires_login, response_failure, response_success


web_api = flask.Blueprint("web_api", __name__)

######################
# USER API ENDPOINTS #
######################
@web_api.route("/user")
def list_users():
    pass


@web_api.route("/user/<username>", methods=["GET"])
def get_user(username):
    pass


@web_api.route("/user/<username>", methods=["POST"])
def create_user(username):
    # TODO: investigate how this works with OAuth/Github (this endpoint redirects to Github?)
    pass


@web_api.route("/user/<username>", methods=["PUT"])
@requires_login
def get_user(username):
    pass


@web_api.route("/user/<username>", methods=["DELETE"])
@requires_login
def delete_user(username):
    pass


# ---------------------- #
# USER BOT API ENDPOINTS #
# ---------------------- #
@web_api.route("/user/<username>/bot", methods=["GET"])
def list_user_bots(username):
    pass


@web_api.route("/user/<username>/bot/<bot_id>", methods=["GET"])
def get_user_bot(username, bot_id):
    pass


@web_api.route("/user/<username>/bot/<bot_id>", methods=["PUT"])
@web_api.route("/user/<username>/bot/<bot_id>", methods=["POST"])
@requires_login
def store_user_bot(user_id, username, bot_id):
    """Store an uploaded bot in object storage."""
    if not config.COMPETITION_OPEN:
        return response_failure("Sorry, but bot submissions are closed.")

    conn = model.engine.connect()
    user = conn.execute(model.users.select(model.users.c.userID == user_id)) \
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
    update = model.users.update() \
        .where(model.users.c.userID == user_id) \
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


@web_api.route("/user/<username>/bot/<bot_id>", methods=["DELETE"])
@requires_login
def delete_user_bot(username, bot_id):
    pass


# ------------------------ #
# USER MATCH API ENDPOINTS #
# ------------------------ #
@web_api.route("/user/<username>/match", methods=["GET"])
def list_user_matches(username):
    pass


@web_api.route("/user/<username>/match/<match_id>", methods=["GET"])
def get_user_match(username, match_id):
    pass


##############################
# ORGANIZATION API ENDPOINTS #
##############################
@web_api.route("/organization")
def list_organizations():
    pass


@web_api.route("/organization/<name>", methods=["GET"])
def get_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["POST"])
def create_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["PUT"])
def update_organization(name):
    pass


@web_api.route("/organization/<name>", methods=["DELETE"])
def delete_organization(name):
    pass


#############################
# LEADERBOARD API ENDPOINTS #
#############################
@web_api.route("/leaderboard")
def leaderboard():
    pass

import flask
import sqlalchemy

from .. import model


web_api = flask.Blueprint("web_api", __name__)


@web_api.route("/botFile", methods=["POST"])
def upload_bot():
    """Store an uploaded bot in object storage."""
    return ""

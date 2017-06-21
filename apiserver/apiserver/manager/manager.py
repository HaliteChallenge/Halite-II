import flask
import sqlalchemy

from .. import model


manager_api = flask.Blueprint("manager_api", __name__)


@manager_api.route("/task")
def task():
    return ""


@manager_api.route("/bots", methods=["POST"])
def upload_bot():
    pass


@manager_api.route("/games", methods=["POST"])
def upload_game():
    pass
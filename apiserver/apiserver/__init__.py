import functools

import flask
from flask import Flask

from . import util


app = Flask(__name__)
# TODO: make this configurable
app.config["MAX_CONTENT_LENGTH"] = 20 * 1024 * 1024
app.secret_key = "development"
app.errorhandler(util.APIError)(util.handle_api_error)

# Helpers


def response_success(more=None):
    response = {
        "status": "success",
    }
    if more is not None:
        response.update(more)
    return flask.jsonify(response)


def requires_login(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" not in flask.session:
            return flask.abort(401)
        kwargs["user_id"] = flask.session["user_id"]
        return view(*args, **kwargs)

    return decorated_view


def optional_login(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" in flask.session:
            kwargs["user_id"] = flask.session["user_id"]
        else:
            kwargs["user_id"] = None
        return view(*args, **kwargs)

    return decorated_view


def requires_admin(view):
    # TODO: NOT IMPLEMENTED!
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        return view(*args, **kwargs)

    return decorated_view


@app.route('/login')
def dev_login():
    # TODO: THIS IS FOR DEVELOPMENT ONLY
    flask.session["user_id"] = int(flask.request.values["user_id"])
    return flask.redirect("/")


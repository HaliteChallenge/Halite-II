import functools

import flask
from flask import Flask


app = Flask(__name__)
# TODO: make this configurable
app.config["MAX_CONTENT_LENGTH"] = 20 * 1024 * 1024
app.secret_key = "development"

# Helpers


def response_failure(reason):
    return flask.jsonify({
        "status": "failure",
        "reason": reason,
    })


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
            return flask.redirect("/login")
        return view(*args, **kwargs, user_id=flask.session["user_id"])

    return decorated_view


def requires_login_api(view):
    @functools.wraps(view)
    def decorated_view(*args, **kwargs):
        if "user_id" not in flask.session:
            return flask.abort(401)
        return view(*args, **kwargs, user_id=flask.session["user_id"])

    return decorated_view


@app.route('/login')
def login():
    flask.session["user_id"] = 2609
    return flask.redirect("/")


@app.route('/user')
@requires_login
def user(*, user_id):
    return str(user_id)


from . import views
from . import manager
from . import web


app.register_blueprint(manager.manager_api, url_prefix="/manager")
app.register_blueprint(web.web_api, url_prefix="/api/web")

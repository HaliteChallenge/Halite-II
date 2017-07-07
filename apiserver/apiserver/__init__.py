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


@app.route('/health_check')
def health_check():
    from . import model
    import sqlalchemy
    with model.engine.connect() as conn:
        conn.execute(sqlalchemy.sql.text("select 1"))
        return ""


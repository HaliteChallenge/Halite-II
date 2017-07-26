import flask
import math

from flask_cors import cross_origin as flask_cross_origin

from . import config


class APIError(Exception):
    """
    Based on http://flask.pocoo.org/docs/0.12/patterns/apierrors/
    """
    def __init__(self, status_code, *, message=None, body=None):
        super().__init__()

        self.status_code = status_code
        self.message = message
        self.body = body

    def to_dict(self):
        result = dict(self.body or ())
        result["status"] = "failure"
        if self.message is not None:
            result["message"] = self.message

        return result


def cross_origin(*args, **kwargs):
    kwargs["origins"] = config.CORS_ORIGINS
    kwargs["supports_credentials"] = True
    kwargs["allow_headers"] = ["Origin", "Accept", "Content-Type"]
    return flask_cross_origin(*args, **kwargs)


@cross_origin(methods=["GET", "POST", "PUT", "OPTIONS"])
def handle_api_error(error):
    """
    The Flask error handler for APIErrors. Use with @app.errorhandler.

    :param error:
    :return:
    """
    response = flask.jsonify(error.to_dict())
    response.status_code = error.status_code
    return response


def tier(rank, total_users):
    for tier, percentage in (("Diamond", config.DIAMOND),
                             ("Platinum", config.PLATINUM),
                             ("Gold", config.GOLD),
                             ("Silver", config.SILVER)):
        num_players = math.ceil(percentage * total_users)
        if rank <= num_players:
            return tier
        else:
            rank -= num_players

    return "Salt"

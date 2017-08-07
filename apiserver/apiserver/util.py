import math
import urllib.parse

import flask
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
    """Mark a view as cross-origin accessible."""
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
    """Given a rank (1-based), return the tier name."""
    thresholds = tier_thresholds(total_users)
    for tier, threshold in sorted(thresholds.items(), key=lambda item: item[1]):
        if rank <= threshold:
            return tier

    return config.TIER_4_NAME


def tier_thresholds(total_users):
    """Return the lowest rank that fits each tier."""
    num_players = 0
    result = {}
    for tier, percentage in ((config.TIER_0_NAME, config.TIER_0_PERCENT),
                             (config.TIER_1_NAME, config.TIER_1_PERCENT),
                             (config.TIER_2_NAME, config.TIER_2_PERCENT),
                             (config.TIER_3_NAME, config.TIER_3_PERCENT)):
        num_players += math.ceil(percentage * total_users)
        result[tier] = num_players

    return result


def response_success(more=None, status_code=200):
    """
    Build a JSON response for a successful REST call.

    :param more: Additional data to be included in the JSON.
    :param status_code: The status code to use.
    :return: The Flask response (tuple of JSON and status code)
    """
    response = {
        "status": "success",
    }
    if more is not None:
        response.update(more)
    return flask.jsonify(response), status_code


def build_site_url(page, params, base_url=config.SITE_URL):
    """

    :param page:
    :param params:
    :param base_url:
    :return:
    """

    return "{}?{}".format(
        urllib.parse.urljoin(base_url, page),
        urllib.parse.urlencode(params))

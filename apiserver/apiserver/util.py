import flask

from flask_cors import cross_origin


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


@cross_origin(methods=["GET", "POST", "PUT", "OPTIONS"],
              supports_credentials=True)
def handle_api_error(error):
    """
    The Flask error handler for APIErrors. Use with @app.errorhandler.

    :param error:
    :return:
    """
    response = flask.jsonify(error.to_dict())
    response.status_code = error.status_code
    return response

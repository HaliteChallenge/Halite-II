"""
Match API endpoints - list matches and get replays/error logs
"""

from .blueprint import web_api

from . import user_match


@web_api.route("/match")
def list_matches():
    # TODO: move list_user_matches impl here, parameterized by (optional) user
    pass


@web_api.route("/match/<int:match_id>")
def get_match(match_id):
    return user_match.get_user_match(0, match_id)
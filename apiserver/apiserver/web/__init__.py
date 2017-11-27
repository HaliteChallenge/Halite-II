from .blueprint import web_api

# Make sure views get registered
from . import hackathon
from . import leaderboard
from . import match
from . import organization
from . import user, user_bot, user_challenge, user_hackathon, user_match
from . import views

from passlib.context import CryptContext

COMPETITION_OPEN = True
# Original PHP: "compState", "finalsPairing"
COMPETITION_FINALS_PAIRING = False
# Max number of games a bot version can error out in before being
# stopped from playing
MAX_ERRORS_PER_BOT = 25

MAX_BOT_UPLOAD_SIZE = 20 * 1024 * 1024
FLASK_SECRET_KEY = ""

DATABASE_PROJECT_ID = ""
DATABASE_REGION = ""
DATABASE_INSTANCE_NAME = ""
DATABASE_URL = ""

# OAuth
OAUTH_GITHUB_CONSUMER_KEY = ""
OAUTH_GITHUB_CONSUMER_SECRET = ""
CORS_ORIGINS = ["http://lvh.me:4000"]
SITE_URL = "http://35.185.45.87"
API_URL = "http://35.190.3.178"

# Google Cloud
GCLOUD_PROJECT = 'TODO'
GCLOUD_PROJECT_ID = 'TODO'
GCLOUD_ZONE = 'us-central1-c'

GCLOUD_COMPILATION_BUCKET = 'TODO'
GCLOUD_BOT_BUCKET = 'TODO'
GCLOUD_REPLAY_BUCKET = 'TODO'
GCLOUD_ERROR_LOG_BUCKET = 'TODO'

# API Key authentication
api_key_context = CryptContext(
    schemes=["argon2"],
    deprecated="auto",
)

# Discourse SSO
DISCOURSE_SSO_SECRET = b""
DISCOURSE_URL = "https://forums.halite.io/sso"

# SendGrid
SENDGRID_API_KEY = ""
SENDGRID_SANDBOX_MODE = True
# Ranking Tiers
DIAMOND = 1/512
PLATINUM = 1/256
GOLD = 1/128
SILVER = 1/64

from passlib.context import CryptContext

COMPETITION_OPEN = True
# Original PHP: "compState", "finalsPairing"
COMPETITION_FINALS_PAIRING = False
MAX_BOT_UPLOAD_SIZE = 20 * 1024 * 1024

DATABASE_PROJECT_ID = ""
DATABASE_REGION = ""
DATABASE_INSTANCE_NAME = ""
DATABASE_URL = ""

# OAuth
OAUTH_GITHUB_CONSUMER_KEY = ""
OAUTH_GITHUB_CONSUMER_SECRET = ""
CORS_ORIGINS = ["http://lvh.me:4000"]
SITE_URL = ""

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
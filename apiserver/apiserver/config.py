from passlib.context import CryptContext

# General coordinator settings
COMPETITION_OPEN = True
# Original PHP: "compState", "finalsPairing"
COMPETITION_FINALS_PAIRING = False

# Max number of games a bot version can error out in before being
# stopped from playing
MAX_ERRORS_PER_BOT = 50
MAX_ERROR_PERCENTAGE = 0.1

# How many minutes old a compilation job must be to be considered stuck.
COMPILATION_STUCK_THRESHOLD = 30

# Flask settings
# Max size of an upload, in bytes
MAX_BOT_UPLOAD_SIZE = 20 * 1024 * 1024
# Secret key for Flask session cookies
FLASK_SECRET_KEY = ""
# Where to look for API keys
API_KEY_HEADER = "X-Api-Key"
# What session cookie to use
SESSION_COOKIE = "user_id"

# Google Cloud
GCLOUD_PROJECT = 'TODO'
GCLOUD_PROJECT_ID = 'TODO'
GCLOUD_ZONE = 'us-central1-c'

GCLOUD_COMPILATION_BUCKET = 'TODO'
GCLOUD_BOT_BUCKET = 'TODO'
# Replays are saved in different buckets based on player level
GCLOUD_REPLAY_BUCKETS = {
    # 0 is the normal bucket
    0: 'todo',
    # 1 is the bucket for gold and above players
    1: 'todo',
}
GCLOUD_ERROR_LOG_BUCKET = 'TODO'
GCLOUD_DEPLOYED_ARTIFACTS_BUCKET = 'TODO'

# The name of the worker source blob in the object storage bucket.
WORKER_ARTIFACT_KEY = ""

DATABASE_PROJECT_ID = ""
DATABASE_REGION = ""
DATABASE_INSTANCE_NAME = ""
DATABASE_URL = ""

# OAuth
OAUTH_GITHUB_CONSUMER_KEY = ""
OAUTH_GITHUB_CONSUMER_SECRET = ""

# CORS setup
CORS_ORIGINS = []
SITE_URL = ""
API_URL = ""

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
TIER_0_NAME = "Diamond"
TIER_0_PERCENT = 1/512
TIER_1_NAME = "Platinum"
TIER_1_PERCENT = 1/256
TIER_2_NAME = "Gold"
TIER_2_PERCENT = 1/128
TIER_3_NAME = "Silver"
TIER_3_PERCENT = 1/64
TIER_4_NAME = "Salt"
# What tier a player must be in to be eligible for GPUs
GPU_TIER_NAME = TIER_2_NAME

# What tier a player must be in to be eligible to participate in the finals
FINALS_TIER_NAME = TIER_2_NAME

from passlib.context import CryptContext
import arrow

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
FLASK_SECRET_KEY = "0ccd512f8c3493797a23557c32db38e7d51ed74f14fa7580"
# Where to look for API keys
API_KEY_HEADER = "X-Api-Key"
# What session cookie to use
SESSION_COOKIE = "user_id"

# Google Cloud
GCLOUD_PROJECT = 'test-hlt'
GCLOUD_PROJECT_ID = 'test-hlt'
GCLOUD_ZONE = 'us-east1-b'

GCLOUD_COMPILATION_BUCKET = 'halite-2-compiled-bots-georgi'
GCLOUD_BOT_BUCKET = 'halite-2-uploaded-bots-georgi'
# Replays are saved in different buckets based on player level
GCLOUD_REPLAY_BUCKETS = {
    # 0 is the normal bucket
    0: 'halite-2-uploaded-bots-georgi',
    # 1 is the bucket for gold and above players
    1: 'halite-2-gold-replays-georgi',
}
GCLOUD_ERROR_LOG_BUCKET = 'halite-2-error-logs-georgi'
GCLOUD_DEPLOYED_ARTIFACTS_BUCKET = 'halite-2-deployed-artifacts-georgi'

# The name of the worker source blob in the object storage bucket.
WORKER_ARTIFACT_KEY = ""

DATABASE_PROJECT_ID = "test-hlt"
DATABASE_REGION = "us-east1-b"
DATABASE_INSTANCE_NAME = "instance-mysql"
DATABASE_URL = "mysql+pymysql://root:23212823@localhost:3307/halite2"

# OAuth
OAUTH_GITHUB_CONSUMER_KEY = "ca22534a3f5ab901760c"
OAUTH_GITHUB_CONSUMER_SECRET = "6c95dadd66857b9bb4f122d6e5d9ada713485fb4"

# CORS setup
SITE_URL = "http://localhost:4000"
API_URL = "http:/35.201.100.217"
CORS_ORIGINS = [SITE_URL]

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

COMPILATION_SUCCESS_TEMPLATE = ""
COMPILATION_FAILURE_TEMPLATE = ""
VERIFY_EMAIL_TEMPLATE = ""
FIRST_TIMEOUT_TEMPLATE = ""
BOT_DISABLED_TEMPLATE = ""
CONFIRMATION_TEMPLATE = ""

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

# Expected end date of the Halite competition
HALITE_END = arrow.get('2017-10-25T21:00:00')

# Badge_id for default badges
REGISTER_BADGE = 1
SUBMISSION_1_BADGE = 2
SUBMISSION_10_BADGE= 3
SUBMISSION_20_BADGE= 4
SUBMISSION_50_BADGE = 5;
# TIER BADGES
TIER_0_BADGE = 6
TIER_1_BADGE = 7
TIER_2_BADGE = 8
TIER_3_BADGE = 9
TIER_4_BADGE = 10


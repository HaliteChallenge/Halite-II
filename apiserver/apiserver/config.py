from passlib.context import CryptContext

# General coordinator settings
COMPETITION_OPEN = True
# If True, only challenge games are issued, and rankings are not updated.
COMPETITION_CHALLENGE_MODE = False
# Original PHP: "compState", "finalsPairing"
COMPETITION_FINALS_PAIRING = False
# Final open game id, set this to the id of last game played in the open stage
LAST_OPEN_GAME = None
# Rank cutoff schedule during finals
# In each entry the first value is number of games to start the cutoff and
# second value is the rank cutoff to use.
# Following schedule is based on ~4000 bots for 6 days of ~3000 games per hour
FINALS_CUTOFF_SCHEDULE = [
    (0, 4500),     # cutoff initially higher than total number of bots
    (42000, 3500), # All bots should now have 30 games at 14 hours in
    (66000, 2600), # At the start of this phase remaining bots have 50 games
    (75000, 1700), # end of starter bots, 60 games, 25 hours
    (127000, 1000), # 150 games, 42 hours
    (178000, 500), # 300 games, 2.5 days
    (229000, 250), # 600 games, 3 days
    (280000, 150), # 1200 games, 4 days
] # at 6 days the top bots should have 4200+ games with 432000 games in finals
# Alternative example schedule for 5000 bots
_5000_schedule = [ # For 5000 bots, 6 days, 3000 gph
    (0, 5500),
    (53000, 4000), # All bots should now have 30 games at 17.5 hours in
    (81000, 3000), # At the start of this phase remaining bots have 50 games
    (92000, 2000), # end of starter bots, 60 games, 30 hours
    (120000, 1000), # 100 games, 40 hours
    (188000, 500), # 200 games, 2.5 days
    (222000, 250), # 400 games, 3 days
    (255000, 175), # 800 games, 3.5 days
] # silver and up should have 3700+ games by end


# Max number of games a bot version can error out in before being
# stopped from playing
MAX_ERRORS_PER_BOT = 50
MAX_ERROR_PERCENTAGE = 0.1

# How many minutes old a compilation job must be to be considered stuck.
COMPILATION_STUCK_THRESHOLD = 30

# Flask settings
# Max size of an upload, in bytes
MAX_BOT_UPLOAD_SIZE = 20 * 1024 * 1024
# Needs to match corresponding value in worker configuration
MAX_COMPILED_BOT_UPLOAD_SIZE = 100 * 1024 * 1024
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
SITE_URL = ""
API_URL = ""
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

# Emails
COMPILATION_SUCCESS_TEMPLATE = ""
COMPILATION_FAILURE_TEMPLATE = ""
VERIFY_EMAIL_TEMPLATE = ""
FIRST_TIMEOUT_TEMPLATE = ""
BOT_DISABLED_TEMPLATE = ""
CONFIRMATION_TEMPLATE = ""
NEW_SUBSCRIBER_TEMPLATE = ""
INVITE_FRIEND_TEMPLATE = ""

RESEARCH_EMAILS = 10307
GAME_ERROR_MESSAGES = 10445
NEWSLETTERS_ARTICLES = 10447
GOODNEWS_ACCOMPLISHMENTS = 10449

C_NEWSLETTER_SUBSCRIPTION = "Newsletter_Subscription" 
C_REGISTRATION_CONFIRMATION = "Registration Confirmation"
C_BOT_DISABLED = "Bot disabled ="
C_BOT_TIMED_OUT = "Bot timed out"
C_COMPILATION_ERROR = "Compilation error"
C_COMPLIATION_SUCCESS = "Compilation success "
C_EMAIL_VERIFICATION = "Email verification"
C_INVITE_FRIEND = "Invite friend"

# Ranking Tiers
TIER_0_NAME = "Diamond"
TIER_0_PERCENT = 1/512
TIER_1_NAME = "Platinum"
TIER_1_PERCENT = 1/256
TIER_2_NAME = "Gold"
TIER_2_PERCENT = 1/128
TIER_3_NAME = "Silver"
TIER_3_PERCENT = 1/64
TIER_4_NAME = "Bronze"
# What tier a player must be in to be eligible for GPUs
GPU_TIER_NAME = TIER_2_NAME

TIER_0_STAY_BADGE = "A"
TIER_1_STAY_BADGE = "B"
TIER_2_STAY_BADGE = "C"
TIER_3_STAY_BADGE = "D"

# Whether GPU workers should preferentially seed is_gpu_enabled players.
ENFORCE_GPU_SEEDING = False

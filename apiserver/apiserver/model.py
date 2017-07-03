import google.cloud.storage as gcloud_storage
import sqlalchemy

from . import config


# Database setup
engine = sqlalchemy.create_engine(config.DATABASE_URL)
metadata = sqlalchemy.MetaData(bind=engine)

organizations = sqlalchemy.Table("organization", metadata, autoload=True)
organization_email_domains = \
    sqlalchemy.Table("organization_email_domain", metadata, autoload=True)
users = sqlalchemy.Table("user", metadata, autoload=True)
user_notifications = sqlalchemy.Table("user_notification", metadata, autoload=True)
bots = sqlalchemy.Table("bot", metadata, autoload=True)
bot_history = sqlalchemy.Table("bot_history", metadata, autoload=True)
games = sqlalchemy.Table("game", metadata, autoload=True)
game_participants = sqlalchemy.Table("game_participant", metadata, autoload=True)


def get_storage_client():
    return gcloud_storage.Client(project=config.GCLOUD_PROJECT)


def get_compilation_bucket():
    """Get the object storage bucket for bots to be compiled."""
    return get_storage_client().get_bucket(config.GCLOUD_COMPILATION_BUCKET)


def get_bot_bucket():
    """Get the object storage bucket for compiled bots."""
    return get_storage_client().get_bucket(config.GCLOUD_BOT_BUCKET)


def get_replay_bucket():
    """Get the object storage bucket for game replays."""
    return get_storage_client().get_bucket(config.GCLOUD_REPLAY_BUCKET)


def get_error_log_bucket():
    """Get the object storage bucket for game error log files."""
    return get_storage_client().get_bucket(config.GCLOUD_ERROR_LOG_BUCKET)

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


def monkeypatch_text(text):
    text.asc = lambda: sqlalchemy.sql.text(text.text + " ASC")
    text.desc = lambda: sqlalchemy.sql.text(text.text + " DESC")
    return text


ranked_bots = sqlalchemy.sql.select([
    sqlalchemy.sql.text("(@rank:=@rank + 1) AS bot_rank"),
    bots.c.user_id,
    bots.c.id.label("bot_id"),
    bots.c.score,
]).select_from(bots).select_from(sqlalchemy.sql.select([
    sqlalchemy.sql.text("@rank:=0")
]).alias("rn")).order_by(bots.c.score.desc()).alias("ranked_bots")


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

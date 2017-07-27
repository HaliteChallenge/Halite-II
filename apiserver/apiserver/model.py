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


def ranked_bots_query(variable="rank", alias="ranked_bots"):
    return sqlalchemy.sql.select([
        sqlalchemy.sql.text("(@{v}:=@{v} + 1) AS bot_rank".format(v=variable)),
        bots.c.user_id,
        bots.c.id.label("bot_id"),
        bots.c.mu,
        bots.c.score,
        bots.c.games_played,
        bots.c.version_number,
        bots.c.language,
    ]).select_from(bots).select_from(sqlalchemy.sql.select([
        sqlalchemy.sql.text("@{}:=0".format(variable))
    ]).alias("rn")).order_by(bots.c.score.desc()).alias(alias)


ranked_bots = ranked_bots_query()
_func = sqlalchemy.sql.func
# Summary of all users, regardless of whether they have bots
all_users = sqlalchemy.sql.select([
    users.c.id.label("user_id"),
    users.c.username,
    users.c.player_level,
    users.c.organization_id,
    organizations.c.organization_name,
    users.c.country_code,
    users.c.country_subdivision_code,
    users.c.email,
    _func.coalesce(_func.count(), 0).label("num_bots"),
    _func.coalesce(_func.sum(ranked_bots.c.games_played), 0).label("num_games"),
    _func.coalesce(_func.sum(ranked_bots.c.version_number), 0).label("num_submissions"),
    _func.coalesce(_func.max(ranked_bots.c.score), 0).label("score"),
    _func.coalesce(_func.min(sqlalchemy.sql.text("ranked_bots.bot_rank"))).label("rank"),
]).select_from(users.join(
    ranked_bots,
    ranked_bots.c.user_id == users.c.id,
    isouter=True,
    ).join(
    organizations,
    organizations.c.id == users.c.organization_id,
    isouter=True
)).group_by(users.c.id).alias("all_users")


# All users with submitted bots, ranked
ranked_bots_users = sqlalchemy.sql.select([
    users.c.id.label("user_id"),
    users.c.username,
    users.c.player_level,
    users.c.organization_id,
    organizations.c.organization_name,
    users.c.country_code,
    users.c.country_subdivision_code,
    users.c.email,
    ranked_bots.c.bot_id,
    ranked_bots.c.games_played.label("num_games"),
    ranked_bots.c.version_number.label("num_submissions"),
    ranked_bots.c.mu,
    ranked_bots.c.score,
    ranked_bots.c.language,
    # Perform a no-op operation so we can label the column easily
    sqlalchemy.cast(sqlalchemy.sql.text("ranked_bots.bot_rank"), sqlalchemy.Integer).label("rank"),
]).select_from(ranked_bots.join(
    users,
    ranked_bots.c.user_id == users.c.id,
    ).join(
    organizations,
    organizations.c.id == users.c.organization_id,
    isouter=True
)).alias("ranked_bots_users")


# Users, ranked by their best bot
def ranked_users_query(alias="ranked_users"):
    ranked_bots = ranked_bots_query("rurank")
    return sqlalchemy.sql.select([
        users.c.id.label("user_id"),
        users.c.username,
        # Perform a no-op operation so we can label the column easily
        _func.min(sqlalchemy.sql.text("ranked_bots.bot_rank")).label("rank"),
    ]).select_from(
        users.join(ranked_bots, ranked_bots.c.user_id == users.c.id)
    ).group_by(users.c.id).alias(alias)


total_ranked_bots = sqlalchemy.sql.select([
    _func.count()
]).select_from(bots).where(bots.c.games_played > 0)


def get_storage_client():
    return gcloud_storage.Client(project=config.GCLOUD_PROJECT)


def get_compilation_bucket():
    """Get the object storage bucket for bots to be compiled."""
    return get_storage_client().get_bucket(config.GCLOUD_COMPILATION_BUCKET)


def get_bot_bucket():
    """Get the object storage bucket for compiled bots."""
    return get_storage_client().get_bucket(config.GCLOUD_BOT_BUCKET)


def get_replay_bucket(kind=0):
    """Get the object storage bucket for game replays."""
    return get_storage_client().get_bucket(config.GCLOUD_REPLAY_BUCKETS[kind])


def get_error_log_bucket():
    """Get the object storage bucket for game error log files."""
    return get_storage_client().get_bucket(config.GCLOUD_ERROR_LOG_BUCKET)

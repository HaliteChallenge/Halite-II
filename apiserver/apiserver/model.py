import enum

import google.cloud.storage as gcloud_storage
import sqlalchemy

from . import config


class CompileStatus(enum.Enum):
    """The compilation status of a bot."""
    UPLOADED = "Uploaded"
    IN_PROGRESS = "InProgress"
    SUCCESSFUL = "Successful"
    FAILED = "Failed"
    DISABLED = "Disabled"


class ChallengeStatus(enum.Enum):
    """The status of a challenge."""
    CREATED = "created"
    PLAYING_GAME = "playing_game"
    FINISHED = "finished"


# Database setup
engine = sqlalchemy.create_engine(config.DATABASE_URL)
metadata = sqlalchemy.MetaData(bind=engine)

organizations = sqlalchemy.Table("organization", metadata, autoload=True)
organization_email_domains = \
    sqlalchemy.Table("organization_email_domain", metadata, autoload=True)
users = sqlalchemy.Table("user", metadata, autoload=True)
halite_1_users = sqlalchemy.Table("halite_1_user", metadata, autoload=True)
leagues = sqlalchemy.Table("leagues", metadata, autoload=True)
user_notifications = sqlalchemy.Table("user_notification", metadata, autoload=True)
bots = sqlalchemy.Table("bot", metadata, autoload=True)
bot_history = sqlalchemy.Table("bot_history", metadata, autoload=True)
games = sqlalchemy.Table("game", metadata, autoload=True)
game_stats = sqlalchemy.Table("game_stat", metadata, autoload=True)
game_view_stats = sqlalchemy.Table("game_view_stat", metadata, autoload=True)
game_bot_stats = sqlalchemy.Table("game_bot_stat", metadata, autoload=True)
game_participants = sqlalchemy.Table("game_participant", metadata, autoload=True)
hackathons = sqlalchemy.Table("hackathon", metadata, autoload=True)
hackathon_participants = sqlalchemy.Table("hackathon_participant", metadata, autoload=True)
hackathon_snapshot = sqlalchemy.Table("hackathon_snapshot", metadata, autoload=True)
challenges = sqlalchemy.Table("challenge", metadata, autoload=True)
challenge_participants = sqlalchemy.Table("challenge_participant", metadata, autoload=True)

def ranked_bots_query(variable="rank", alias="ranked_bots"):
    """
    Builds a query that ranks all bots.

    This is a function in case you need this as a subquery multiple times,
    and would like to avoid reusing the same SQL variable.

    Unfortunately, MySQL does not support SQL variables in views.
    """
    return sqlalchemy.sql.select([
        sqlalchemy.sql.text("(@{v}:=@{v} + 1) AS bot_rank".format(v=variable)),
        bots.c.user_id,
        bots.c.id.label("bot_id"),
        bots.c.mu,
        bots.c.sigma,
        bots.c.score,
        bots.c.games_played,
        bots.c.version_number,
        bots.c.language,
        bots.c.update_time,
        bots.c.compile_status,
    ]).select_from(bots).select_from(sqlalchemy.sql.select([
        sqlalchemy.sql.text("@{}:=0".format(variable))
    ]).alias("rn")).order_by(bots.c.score.desc()).alias(alias)


def hackathon_ranked_bots_query(hackathon_id,
                                *,
                                variable="hrank",
                                alias="hackathon_ranked_bots"):
    """
    Builds a query that ranks all bots within a given hackathon.
    """

    temptable = sqlalchemy.sql.select([
        hackathon_snapshot.c.user_id,
        hackathon_snapshot.c.bot_id,
        hackathon_snapshot.c.score,
        hackathon_snapshot.c.mu,
        hackathon_snapshot.c.sigma,
        hackathon_snapshot.c.games_played,
        hackathon_snapshot.c.version_number,
        hackathon_snapshot.c.language,
    ]).select_from(
        hackathon_snapshot
    ).select_from(sqlalchemy.sql.select([
        sqlalchemy.sql.text("@{}:=0".format(variable))
    ]).alias("rn")).where(
        hackathon_snapshot.c.hackathon_id == hackathon_id
    ).order_by(hackathon_snapshot.c.score.desc()).alias("temptable")

    return sqlalchemy.sql.select([
        sqlalchemy.sql.text("(@{v}:=@{v} + 1) AS local_rank".format(v=variable)),
        temptable.c.user_id,
        temptable.c.bot_id,
        temptable.c.mu,
        temptable.c.sigma,
        temptable.c.score,
        temptable.c.games_played,
        temptable.c.version_number,
        temptable.c.language,
    ]).select_from(temptable).alias(alias)


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
    users.c.github_email.label("email"),
    users.c.email.label("personal_email"),
    users.c.is_email_good,
    users.c.is_gpu_enabled,
    _func.coalesce(_func.count(), 0).label("num_bots"),
    _func.coalesce(_func.sum(ranked_bots.c.games_played), 0).label("num_games"),
    _func.coalesce(_func.sum(ranked_bots.c.version_number), 0).label("num_submissions"),
    _func.coalesce(_func.max(ranked_bots.c.score), 0).label("score"),
    _func.coalesce(_func.max(ranked_bots.c.sigma), 0).label("sigma"),
    _func.coalesce(_func.max(ranked_bots.c.mu), 0).label("mu"),
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


# All submitted bots, ranked with user info
ranked_bots_users = sqlalchemy.sql.select([
    users.c.id.label("user_id"),
    users.c.username,
    users.c.player_level,
    users.c.organization_id,
    organizations.c.organization_name,
    users.c.country_code,
    users.c.country_subdivision_code,
    users.c.github_email.label("email"),
    users.c.is_gpu_enabled,
    ranked_bots.c.bot_id,
    ranked_bots.c.games_played.label("num_games"),
    ranked_bots.c.version_number.label("num_submissions"),
    ranked_bots.c.mu,
    ranked_bots.c.sigma,
    ranked_bots.c.score,
    ranked_bots.c.language,
    ranked_bots.c.update_time,
    # Perform a no-op operation so we can label the column easily
    sqlalchemy.cast(sqlalchemy.sql.text("ranked_bots.bot_rank"), sqlalchemy.Integer).label("rank"),
    ranked_bots.c.compile_status,
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


# Total number of ranked users that have played a game
total_ranked_users = sqlalchemy.sql.select([
    _func.count(sqlalchemy.distinct(bots.c.user_id))
]).select_from(bots).where(bots.c.games_played > 0)


def hackathon_total_ranked_users_query(hackathon_id):
    """Build a query counting all users in a hackathon."""
    return sqlalchemy.sql.select([
        _func.count(sqlalchemy.distinct(bots.c.user_id))
    ]).select_from(
        bots.join(
            hackathon_participants,
            (bots.c.user_id == hackathon_participants.c.user_id) &
            (hackathon_participants.c.hackathon_id == hackathon_id)
        ).join(
            users,
            (bots.c.user_id == users.c.id) &
            (users.c.is_email_good == True)
        )
    ).where(bots.c.games_played > 0)


def hackathon_ranked_bots_users_query(hackathon_id, *, alias="hackathon_ranked_bots_users"):
    """Build a query that ranks all users in a hackathon by their best bot."""
    local_rank = hackathon_ranked_bots_query(hackathon_id, alias="local_rank")
    return sqlalchemy.sql.select([
        users.c.id.label("user_id"),
        users.c.username,
        users.c.player_level,
        users.c.organization_id,
        organizations.c.organization_name,
        users.c.country_code,
        users.c.country_subdivision_code,
        ranked_bots.c.bot_id,
        local_rank.c.games_played.label("num_games"),
        local_rank.c.version_number.label("num_submissions"),
        local_rank.c.mu,
        local_rank.c.score,
        local_rank.c.language,
        ranked_bots.c.update_time,
        # Perform a no-op operation so we can label the column easily
        sqlalchemy.cast(sqlalchemy.sql.text("local_rank.local_rank"), sqlalchemy.Integer).label("local_rank"),
        ranked_bots.c.compile_status,
    ]).select_from(
        ranked_bots.join(
            users,
            (ranked_bots.c.user_id == users.c.id) &
            # Only include verified users
            (users.c.is_email_good == True),
        ).join(
            local_rank,
            (local_rank.c.user_id == ranked_bots.c.user_id) &
            (local_rank.c.bot_id == ranked_bots.c.bot_id)
        ).join(
            organizations,
            organizations.c.id == users.c.organization_id,
            isouter=True
        )
    ).alias(alias)


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


def get_deployed_artifacts_bucket():
    """Get the object storage bucket for deployed worker artifacts."""
    return get_storage_client().get_bucket(
        config.GCLOUD_DEPLOYED_ARTIFACTS_BUCKET)

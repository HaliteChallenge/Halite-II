import google.cloud.storage as gcloud_storage
import sqlalchemy

from . import config


# Database setup
# TODO: make this configurable
engine = sqlalchemy.create_engine("mysql+pymysql://halite2:password@localhost/halite2")
metadata = sqlalchemy.MetaData(bind=engine)
users = sqlalchemy.Table("User", metadata, autoload=True)
user_history = sqlalchemy.Table("UserHistory", metadata, autoload=True)


def get_storage_client():
    return gcloud_storage.Client(project=config.GCLOUD_PROJECT)


def get_compilation_bucket():
    """Get the object storage bucket for bots to be compiled."""
    return get_storage_client().get_bucket(config.GCLOUD_COMPILATION_BUCKET)


def get_bot_bucket():
    """Get the object storage bucket for compiled bots."""
    return get_storage_client().get_bucket(config.GCLOUD_BOT_BUCKET)

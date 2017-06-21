import sqlalchemy


# Database setup
# TODO: make this configurable
engine = sqlalchemy.create_engine("mysql+pymysql://halite2:password@localhost/halite2")
metadata = sqlalchemy.MetaData(bind=engine)
users = sqlalchemy.Table("User", metadata, autoload=True)

import logging
import logging.handlers

from flask import Flask

from . import badge_util, config, util


app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = config.MAX_BOT_UPLOAD_SIZE
app.secret_key = config.FLASK_SECRET_KEY
app.errorhandler(util.APIError)(util.handle_api_error)


@app.route('/health_check')
def health_check():
    from . import model
    import sqlalchemy
    with model.engine.connect() as conn:
        conn.execute(sqlalchemy.sql.text("select 1"))
        return ""


def log_exception(sender, exception, **extra):
    pass


logging.basicConfig(level=logging.DEBUG)


def setup_logging(log_name, logger):
    handler = logging.handlers.RotatingFileHandler(log_name, maxBytes=1024*1024*20, backupCount=20)
    handler.setLevel(logging.DEBUG)
    logger.addHandler(handler)

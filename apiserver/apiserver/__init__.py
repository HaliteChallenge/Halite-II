import logging
import logging.handlers

import flask
from flask import Flask

from . import config, util


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


def setup_logging(log_name):
    handler = logging.handlers.RotatingFileHandler(log_name, maxBytes=1024*1024*10, backupCount=10)
    handler.setLevel(logging.INFO)
    app.logger.addHandler(handler)
    logging.basicConfig(handlers=[handler], level=logging.INFO)

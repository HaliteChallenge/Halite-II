from . import app, setup_logging
from . import config
from . import coordinator

app.config["MAX_CONTENT_LENGTH"] = config.MAX_COMPILED_BOT_UPLOAD_SIZE

setup_logging("coordinator_server.log", app.logger)
app.register_blueprint(coordinator.coordinator_api, url_prefix="/v1/coordinator")

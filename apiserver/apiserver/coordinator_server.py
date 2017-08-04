from . import app, setup_logging
from . import coordinator


setup_logging("coordinator_server.log", app.logger)
app.register_blueprint(coordinator.coordinator_api, url_prefix="/v1/coordinator")

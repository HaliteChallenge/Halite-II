from . import app, setup_logging
from . import coordinator


setup_logging("coordinator_server.log")
app.register_blueprint(coordinator.coordinator_api, url_prefix="/coordinator/v1")

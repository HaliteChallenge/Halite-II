from . import app
from . import coordinator


app.register_blueprint(coordinator.coordinator_api, url_prefix="/coordinator/v1")

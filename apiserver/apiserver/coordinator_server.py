from . import app
from . import manager


app.register_blueprint(manager.manager_api, url_prefix="/coordinator/v1")

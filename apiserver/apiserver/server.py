from . import app, setup_logging
from . import login
from . import web


setup_logging("api_server.log")
app.register_blueprint(login.oauth_login, url_prefix="/login")
app.register_blueprint(web.web_api, url_prefix="/api/v1")
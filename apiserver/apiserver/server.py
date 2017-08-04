from . import app, setup_logging
from . import login
from . import web

import flask_oauthlib.client


setup_logging("api_server.log", app.logger)
setup_logging("oauth.log", flask_oauthlib.client.log)
setup_logging("login.log", login.login_log)
app.register_blueprint(login.oauth_login, url_prefix="/v1/login")
app.register_blueprint(web.web_api, url_prefix="/v1/api")
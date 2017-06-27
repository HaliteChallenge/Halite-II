from . import app
from . import login
from . import web


app.register_blueprint(login.oauth_login, url_prefix="/login")
app.register_blueprint(web.web_api, url_prefix="/api/v1")
from . import app
from . import login
from . import manager
from . import web


app.register_blueprint(login.oauth_login, url_prefix="/login")
app.register_blueprint(manager.manager_api, url_prefix="/manager")
app.register_blueprint(web.web_api, url_prefix="/api/web")
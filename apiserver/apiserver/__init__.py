from flask import Flask
app = Flask(__name__)


from . import views
from . import manager
from . import web

app.register_blueprint(manager.manager_api, url_prefix="/manager")
app.register_blueprint(web.web_api, url_prefix="/api/web")

"""
Miscellaneous API endpoints
"""

import base64
import hashlib
import hmac
import urllib.parse

import flask

from .. import config, model, util

from .blueprint import web_api
from . import util as api_util


@web_api.route("/login/discourse_sso")
@api_util.requires_login(accept_key=False)
def discourse_sso(*, user_id):
    """
    Implements an SSO endpoint for Discourse forums, as described at
    https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045
    """

    sso_payload = flask.request.args.get("sso")
    sso_signature = flask.request.args.get("sig")
    if not sso_payload or not sso_signature:
        raise util.APIError(400, message="SSO payload and signature required.")

    hmac_obj = hmac.new(config.DISCOURSE_SSO_SECRET,
                        msg=sso_payload,
                        digestmod=hashlib.sha256)
    computed_signature = hmac_obj.hexdigest()
    if computed_signature != sso_signature:
        raise util.APIError(401)

    with model.engine.connect() as conn:
        user = conn.execute(model.users.select().where(
            model.users.c.id == user_id,
        )).first()

        if not user:
            raise util.APIError(401)

    payload = urllib.parse.parse_qs(
        base64.b64decode(urllib.parse.unquote(sso_payload)))
    nonce = payload.get(b"nonce")
    if not nonce:
        raise util.APIError(401)
    nonce = nonce[0]

    raw_payload = {
        "nonce": nonce,
        "email": user["email"],
        "external_id": str(user_id),
        "username": user["username"],
    }
    if user["is_email_good"] != 1:
        raw_payload["require_activation"] = "true"

    encoded_payload = base64.b64encode(
        urllib.parse.urlencode(raw_payload).encode("utf-8")).decode("utf-8")
    new_signature = hmac.new(config.DISCOURSE_SSO_SECRET,
                             msg=encoded_payload,
                             digestmod=hashlib.sha256).hexdigest()
    return flask.redirect(
        config.DISCOURSE_URL +
        "?sso={}&sig={}".format(encoded_payload, new_signature))

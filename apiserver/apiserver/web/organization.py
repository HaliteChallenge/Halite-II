"""
Organization API endpoints - create/update/delete/list organizations
"""
import uuid

import flask
import sqlalchemy

from .. import model, util

from . import util as web_util
from .blueprint import web_api


@web_api.route("/organization")
@util.cross_origin(methods=["GET"])
def list_organizations():
    result = []
    offset, limit = web_util.get_offset_limit()
    where_clause, order_clause, _ = web_util.get_sort_filter({
        "organization_id": model.organizations.c.id,
        "name": model.organizations.c.organization_name,
        "type": model.organizations.c.kind,
    })

    with model.engine.connect() as conn:
        # Don't limit this query
        query = model.organizations.select() \
            .where(where_clause).order_by(*order_clause) \
            .offset(offset)
        orgs = conn.execute(query)

        for org in orgs.fetchall():
            result.append({
                "organization_id": org["id"],
                "name": org["organization_name"],
                "type": org["kind"],
            })

    return flask.jsonify(result)


@web_api.route("/organization/<int:org_id>", methods=["GET"])
@util.cross_origin(methods=["GET"])
def get_organization(org_id):
    with model.engine.connect() as conn:
        org = conn.execute(model.organizations.select().where(
            model.organizations.c.id == org_id
        )).first()

        if not org:
            raise util.APIError(404, message="Organization not found.")

        return flask.jsonify({
            "organization_id": org["id"],
            "name": org["organization_name"],
            "type": org["kind"],
        })


@web_api.route("/organization", methods=["POST"])
@web_util.requires_login(accept_key=True, admin=True)
def create_organization(*, user_id):
    org_body = flask.request.get_json()
    if "name" not in org_body:
        raise util.APIError(400, message="Organization must be named.")

    if "type" not in org_body:
        raise util.APIError(400, message="Organization must have a type.")

    verification_code = None
    if org_body.get("require_code", False):
        verification_code = uuid.uuid4().hex

    with model.engine.connect() as conn:
        org_id = conn.execute(model.organizations.insert().values(
            organization_name=org_body["name"],
            organization_kind=org_body["type"],
            verification_code=verification_code,
        )).inserted_primary_key[0]

    response = {
        "organization_id": org_id[0],
    }
    if verification_code:
        response["verification_code"] = verification_code
    return util.response_success(response, status_code=201)


@web_api.route("/organization/<int:org_id>", methods=["PUT"])
@web_util.requires_login(accept_key=True, admin=True)
def update_organization(org_id, *, user_id):
    fields = flask.request.get_json()
    columns = {
        "name": model.organizations.c.organization_name,
        "type": model.organizations.c.kind,
    }

    for key in fields:
        if key not in columns:
            raise util.APIError(400, message="Cannot update '{}'".format(key))

    with model.engine.connect() as conn:
        conn.execute(model.organizations.update().where(
            model.organizations.c.id == org_id
        ).values(**fields))

    return util.response_success()


@web_api.route("/organization/<int:org_id>/email_domain", methods=["GET"])
@web_util.requires_login(accept_key=True, admin=True)
def list_organization_email_domains(org_id, *, user_id):
    result = []
    with model.engine.connect() as conn:
        domains = conn.execute(model.organization_email_domains.select(
            model.organization_email_domains.c.organization_id == org_id
        ))

        for domain in domains.fetchall():
            result.append(domain["domain"])

    return flask.jsonify(result)


@web_api.route("/organization/<int:org_id>/email_domain", methods=["POST"])
@web_util.requires_login(accept_key=True, admin=True)
def create_organization_email_domain(org_id, *, user_id):
    domains = []

    json_body = flask.request.get_json()
    if json_body:
        if "domain" in json_body:
            domains.append(json_body["domain"])
        elif "domains" in json_body:
            domains.extend(json_body["domains"])
    else:
        if "domain" in flask.request.form:
            domains.append(flask.request.form["domain"])
        elif "domains" in flask.request.form:
            domains.extend(flask.request.form.getlist("domain"))

    with model.engine.connect() as conn:
        org = conn.execute(model.organizations.select().where(
            model.organizations.c.id == org_id
        )).first()

        if org is None:
            raise util.APIError(404, message="Organization does not exist.")

        conn.execute(
            model.organization_email_domains.insert(),
            [
                {
                    "organization_id": org_id,
                    "domain": domain,
                }
                for domain in domains
            ]
        )

    return util.response_success(status_code=201)


@web_api.route("/organization/<int:org_id>", methods=["DELETE"])
@web_util.requires_login(accept_key=True, admin=True)
def delete_organization(org_id, *, user_id):
    with model.engine.connect() as conn:
        with conn.begin() as transaction:
            count = conn.execute(sqlalchemy.sql.select([
                sqlalchemy.sql.func.count()
            ]).select_from(model.users).where(
                model.users.c.id == org_id
            )).first()[0]

            if count > 0:
                raise util.APIError(
                    400, message="Cannot delete organization with members.")

            conn.execute(model.organizations.delete().where(
                model.organizations.c.id == org_id
            ))

    return util.response_success()

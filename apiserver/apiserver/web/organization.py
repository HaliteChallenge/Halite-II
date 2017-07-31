"""
Organization API endpoints - create/update/delete/list organizations
"""

import flask
import sqlalchemy

from .. import model, util
from .. import response_success
from ..util import cross_origin

from .util import get_offset_limit, get_sort_filter, requires_admin
from .blueprint import web_api


@web_api.route("/organization")
@cross_origin(methods=["GET"])
def list_organizations():
    result = []
    offset, limit = get_offset_limit()
    where_clause, order_clause, _ = get_sort_filter({
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
@cross_origin(methods=["GET"])
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
@requires_admin(accept_key=True)
def create_organization(*, admin_id):
    org_body = flask.request.get_json()
    if "name" not in org_body:
        raise util.APIError(400, message="Organization must be named.")

    if "type" not in org_body:
        raise util.APIError(400, message="Organization must have a type.")

    with model.engine.connect() as conn:
        org_id = conn.execute(model.organizations.insert().values(
            organization_name=org_body["name"],
            organization_kind=org_body["type"],
        )).inserted_primary_key[0]

    return response_success({
        "organization_id": org_id[0],
    })


@web_api.route("/organization/<int:org_id>", methods=["PUT"])
@requires_admin(accept_key=True)
def update_organization(org_id, *, admin_id):
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

    return response_success()


@web_api.route("/organization/<int:org_id>", methods=["DELETE"])
@requires_admin(accept_key=True)
def delete_organization(org_id, *, admin_id):
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

    return response_success()
import sqlalchemy
import threading
import requests
from . import model, config

HACK_FIRST = "First"
HACK_SECOND = "Second"
HACK_THIRD = "Third"


def add_badge_if_not_exist(user_id, badge_id):
    ret = requests.get("http://127.0.0.1:5000/v1/api/user/{}/badge/{}"
             .format(user_id, badge_id))
    if ret.status_code == 404:
        requests.post(
            url="http://127.0.0.1:5000/v1/api/user/{}/badge".format(user_id),
            json={"badge_id": badge_id},
        )


def transalte_name_to_badge_id(name, add_if_not_exist=False):
    with model.engine.connect() as conn:
        badge = conn.execute(model.badge.select().where(
            sqlalchemy.sql.func.lower(model.badge.c.name) ==
            sqlalchemy.sql.func.lower(name))).first()
        if not badge and add_if_not_exist:
            badge = requests.post(
                    url="http://127.0.0.1:5000/v1/api/badge",
                    json={"name": name}
            ).json()
            badge = ret;
        return badge['id']


def __add_user_badge(user_id, badge_name):
    badge_id = transalte_name_to_badge_id(
        badge_name,
        add_if_not_exist=True
    )
    print(badge_id)
    add_badge_if_not_exist(user_id, badge_id)


def add_successful_submission_badge(user_id, badge_name,
                                    submissions_count, version_number):
    if version_number >= submissions_count:
        threading.Thread(
            target=__add_user_badge,
            args=(user_id, badge_name)
        ).start()

def add_registration_badge(user_id):
    threading.Thread(
        target=__add_user_badge,
        args=(user_id, config.REGISTER_BADGE)
    ).start()


def init_hackathon_badges(hackathon_title, hackathon_id):
    for badge_type in [HACK_FIRST, HACK_SECOND, HACK_THIRD]:
        threading.Thread(
            target=requests.post,
            kwargs=({
                "url": "http://127.0.0.1:5000/v1/api/badge",
                "json": {
                    "name": get_hackathon_badge_name(hackathon_title, badge_type),
                    "family": "hackathon",
                    "family_id": 
                        get_hackathon_badge_family_id(hackathon_id, badge_type),
                }})).start()


def get_hackathon_badge_family_id(hackathon_id, badge_type):
    return "{}_{}".format(hackathon_id, badge_type)

def get_hackathon_badge_name(hackathon_title, badge_type):
    return "{} {} Place".format(hackathon_title, badge_type)


def __update_hackathon_badge(hackathon_title, hackathon_id, badge_type):
    resp = requests.get(url="http://127.0.0.1:5000/v1/api/badge").json()
    print(resp)
    print(get_hackathon_badge_family_id(hackathon_id, badge_type))
    for badge in resp:
        if badge['family_id'] == \
            get_hackathon_badge_family_id(hackathon_id, badge_type):
            requests.put(
                url="http://127.0.0.1:5000/v1/api/badge/{}"
                    .format(badge['id']),
                json = {
                    "name":get_hackathon_badge_name(hackathon_title, badge_type)
                })


def update_hackathon_badges(new_hackathon_title, hackathon_id):
    for badge_type in [HACK_FIRST, HACK_SECOND, HACK_THIRD]:
        threading.Thread(
            target=__update_hackathon_badge,
            args=(new_hackathon_title, hackathon_id, badge_type),
        ).start()

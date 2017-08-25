import threading
import requests
from . import config


def add_badge_if_not_exist(user_id, badge_id):
    ret = requests.get("http://127.0.0.1:5000/v1/api/user/{}/badge/{}"
             .format(user_id, badge_id))
    if ret.status_code == 404:
        requests.post(
            url="http://127.0.0.1:5000/v1/api/user/{}/badge".format(user_id),
            json={"badge_id": badge_id},
        )


def add_successful_submission_badge(user_id, badge_id,
                                    submissions_count, version_number):
    if version_number >= submissions_count:
        threading.Thread(
            target=add_badge_if_not_exist,
            args=(user_id, badge_id)
        ).start()


def add_registration_badge(user_id):
    threading.Thread(
        target=requests.post,
        kwargs=({
            "url": "http://127.0.0.1:5000/v1/api/user/{}/badge"
                .format(user_id),
            "json": {"badge_id": config.REGISTER_BADGE},
        })
    ).start()

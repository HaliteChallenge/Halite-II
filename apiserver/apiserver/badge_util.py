import sqlalchemy
import threading
import requests
from urllib.parse import urljoin
from . import config, model
from .web import badges


HACK_FIRST = "First"
HACK_SECOND = "Second"
HACK_THIRD = "Third"
FLASK_API = "http://127.0.0.1:5000/v1/api/"
URL_HACK_LEADERBOARD = urljoin(FLASK_API, "hackathon/{}/leaderboard?limit={}")
URL_USER_BADGE_ID = urljoin(FLASK_API, "user/{}/badge/{}")
URL_USER_BADGE = urljoin(FLASK_API, "user/{}/badge")
URL_BADGE = urljoin(FLASK_API, "badge")
URL_HACKATHON = urljoin(FLASK_API, "hackathon")
URL_LEADERBOARD  = urljoin(FLASK_API, "leaderboard?offset={}&limit={}")
TIERS = [config.TIER_0_NAME, config.TIER_1_NAME, config.TIER_2_NAME,
         config.TIER_3_NAME, config.TIER_4_NAME]
TIER_BADGES = [config.TIER_0_STAY_BADGE, config.TIER_1_STAY_BADGE,
               config.TIER_2_STAY_BADGE, config.TIER_3_STAY_BADGE]


def get_temp_tier_badge_name(tier_name):
    return config.TIER_TEMP_BADGE.format(tier_name)


def assign_badges_for_hackathon(hackathon_id, hackathon_name):
    """Assign the badges for the first three ranks of a hackahton."""
    ret = requests.get(URL_HACK_LEADERBOARD.format(hackathon_id, 3)).json()
    if len(ret) > 0:
        _add_user_badge(
            ret[0][badges._USER_KEY],
            get_hackathon_badge_name(hackathon_name, HACK_FIRST),
        )
    if len(ret) > 1:
        _add_user_badge(
            ret[1][badges._USER_KEY],
            get_hackathon_badge_name(hackathon_name, HACK_SECOND),
        )
    if len(ret) > 2:
        _add_user_badge(
            ret[2][badges._USER_KEY],
            get_hackathon_badge_name(hackathon_name, HACK_THIRD),
        )


def add_badge_if_not_exist(user_id, badge_id):
    """ Assign user a badge if not assigned already."""
    ret = requests.get(URL_USER_BADGE_ID.format(user_id, badge_id))
    if ret.status_code == 404:
        requests.post(
            url=URL_USER_BADGE.format(user_id),
            json={badges._BADGE_KEY: badge_id},
        )


def translate_name_to_badge_id(name, add_if_not_exist=False):
    """Translate the name of a badge to the actual badge. Case ignored."""
    with model.engine.connect() as conn:
        badge = conn.execute(model.badge.select().where(
            sqlalchemy.sql.func.lower(model.badge.c.name) ==
            sqlalchemy.sql.func.lower(name))).first()
        if not badge and add_if_not_exist:
            badge = requests.post( url=URL_BADGE, json={badges._NAME_KEY: name}).json()
        return badge[badges._ID_KEY]


def _add_user_badge(user_id, badge_name):
    badge_id = translate_name_to_badge_id(
        badge_name,
        add_if_not_exist=True
    )
    add_badge_if_not_exist(user_id, badge_id)


def add_successful_submission_badge(user_id, badge_name,
                                    submissions_count, version_number):
    if version_number >= submissions_count:
        threading.Thread(
            target=_add_user_badge,
            args=(user_id, badge_name)
        ).start()

def add_registration_badge(user_id):
    threading.Thread(
        target=_add_user_badge,
        args=(user_id, config.REGISTER_BADGE)
    ).start()


def init_hackathon_badges(hackathon_title, hackathon_id):
    for badge_type in [HACK_FIRST, HACK_SECOND, HACK_THIRD]:
        threading.Thread(
            target=requests.post,
            kwargs=({
                "url": URL_BADGE,
                "json": {
                    badges._NAME_KEY: get_hackathon_badge_name(hackathon_title, badge_type),
                }})).start()


def get_hackathon_badge_name(hackathon_title, badge_type):
    return config.HACKATHON_RANK_BADGE.format(hackathon_title, badge_type)


def _update_hackathon_badge(new_hackathon_title, old_hackathon_title, badge_type):
    with model.engine.connext() as conn:
        conn.execute(model.badge.update().where(
            sqlalchemy.sql.func.lower(model.badge.c.name) ==
            get_hackathon_badge_name(old_hackathon_title, badge_type)
        ).values(name=get_hackathon_badge_name(new_hackathon_title, badge_type)))


def update_hackathon_badges(new_hackathon_title, hackathon_id):
    for badge_type in [HACK_FIRST, HACK_SECOND, HACK_THIRD]:
        threading.Thread(
            target=_update_hackathon_badge,
            args=(new_hackathon_title, old_hackathon_title, badge_type),
        ).start()


def _update_tier_stay_badge(badge_name, user_stays):
    for user_stay in user_stays:
        _add_user_badge(user_stay[badges._USER_KEY], badge_name)


def replace_temp_tier_badge(user_id, tier_name):
    for tier in TIERS:
        if tier_name == tier:
            continue
        requests.delete(URL_USER_BADGE_ID.format(
            user_id,
            translate_name_to_badge_id(
                get_temp_tier_badge_name(tier),
                add_if_not_exist=True
            )))
    _add_user_badge(
        user_id,
        get_temp_tier_badge_name(tier_name),
    )


def update_tier_stay_badge():
    """For every user update their badge to represent the tier they currently have"""
    with model.engine.connect() as conn:
        res = list(conn.execute(model.user_tier_history.select().where(
            model.user_tier_history.c.total_time_in_tier >= config.STAY_FOR_BADGE
        )))
        for (tier, badge) in zip(TIERS[:-1], TIER_BADGES):
            _update_tier_stay_badge(
                badge, 
                [x for x in res if x['tier'] == tier],
            )

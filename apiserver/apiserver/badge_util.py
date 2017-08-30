import sqlalchemy
import threading
import requests
from . import config, model

HACK_FIRST = "First"
HACK_SECOND = "Second"
HACK_THIRD = "Third"

TIERS = [config.TIER_0_NAME, config.TIER_1_NAME, config.TIER_2_NAME,
         config.TIER_3_NAME, config.TIER_4_NAME]

def get_temp_tier_badge_name(tier_name):
    return config.TIER_TEMP_BADGE.format(tier_name)


def assign_badges_for_hackathon(hackathon_id, hackathon_name):
    badges = None
    ret = requests.get('http://127.0.0.1:5000/v1/api/hackathon/{}/leaderboard?'
        'limit=3'.format(hackathon_id)).json()
    if len(ret) > 0:
        add_badge_if_not_exist(
                ret[0]['user_id'],
                translate_name_to_badge_id(
                    get_hackathon_badge_name(hackathon_name, HACK_FIRST),
                    add_if_not_exist=True,
                ))
    if len(ret) > 1:
        add_badge_if_not_exist(
                ret[1]['user_id'],
                translate_name_to_badge_id(
                    get_hackathon_badge_name(hackathon_name, HACK_SECOND),
                    add_if_not_exist=True,
                ))
    if len(ret) > 2:
        add_badge_if_not_exist(
                ret[2]['user_id'],
                translate_name_to_badge_id(
                    get_hackathon_badge_name(hackathon_name, HACK_THIRD),
                    add_if_not_exist=True,
                ))


def add_badge_if_not_exist(user_id, badge_id):
    ret = requests.get("http://127.0.0.1:5000/v1/api/user/{}/badge/{}"
             .format(user_id, badge_id))
    if ret.status_code == 404:
        requests.post(
            url="http://127.0.0.1:5000/v1/api/user/{}/badge".format(user_id),
            json={"badge_id": badge_id},
        )


def translate_name_to_badge_id(name, add_if_not_exist=False):
    with model.engine.connect() as conn:
        badge = conn.execute(model.badge.select().where(
            sqlalchemy.sql.func.lower(model.badge.c.name) ==
            sqlalchemy.sql.func.lower(name))).first()
        if not badge and add_if_not_exist:
            badge = requests.post(
                    url="http://127.0.0.1:5000/v1/api/badge",
                    json={"name": name}
            ).json()
        return badge['id']


def __add_user_badge(user_id, badge_name):
    badge_id = translate_name_to_badge_id(
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
                }})).start()


def get_hackathon_badge_name(hackathon_title, badge_type):
    return config.HACKATHON_RANK_BADGE.format(hackathon_title, badge_type)


def __update_hackathon_badge(new_hackathon_title, old_hackathon_title, badge_type):
    with model.engine.connext() as conn:
        conn.execute(model.badge.update().where(
            sqlalchemy.sql.func.lower(model.badge.c.name) ==
            get_hackathon_badge_name(old_hackathon_title, badge_type)
        ).values(name=get_hackathon_badge_name(new_hackathon_title, badge_type)))


def update_hackathon_badges(new_hackathon_title, hackathon_id):
    for badge_type in [HACK_FIRST, HACK_SECOND, HACK_THIRD]:
        threading.Thread(
            target=__update_hackathon_badge,
            args=(new_hackathon_title, old_hackathon_title, badge_type),
        ).start()


def __update_tier_stay_badge(badge_name, user_stays):
    print(badge_name, user_stays)
    for user_stay in user_stays:
        __add_user_badge(user_stay['user_id'], badge_name)


def replace_temp_tier_badge(user_id, tier_name):
    for tier in TIERS:
        if tier_name == tier:
            continue
        requests.delete('http://127.0.0.1:5000/v1/api/user/{}/badge/{}'
                .format(
                    user_id,
                    translate_name_to_badge_id(
                        get_temp_tier_badge_name(tier),
                        add_if_not_exist=True
                    )))
    add_badge_if_not_exist(
        user_id,
        translate_name_to_badge_id(
            get_temp_tier_badge_name(tier_name),
            add_if_not_exist=True
        ))


def update_tier_stay_badge():
    with model.engine.connect() as conn:
        res = list(conn.execute(model.user_tier_history.select().where(
            model.user_tier_history.c.total_time_in_tier >= config.STAY_FOR_BADGE
        )))
    __update_tier_stay_badge(
        config.TIER_0_STAY_BADGE,
        [x for x in res if x['tier'] == config.TIER_0_NAME],
    )
    __update_tier_stay_badge(
        config.TIER_1_STAY_BADGE,
        [x for x in res if x['tier'] == config.TIER_1_NAME],
    )
    __update_tier_stay_badge(
        config.TIER_2_STAY_BADGE,
        [x for x in res if x['tier'] == config.TIER_2_NAME],
    )
    __update_tier_stay_badge(
        config.TIER_3_STAY_BADGE,
        [x for x in res if x['tier'] == config.TIER_3_NAME],
    )

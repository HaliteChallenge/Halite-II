from .. import badge_util, model
import requests
import sched
import time

UPDATE_HACKATHON_BADGE_SLEEP = 120

def assign_place_badge(hackathon_id, badge_type, user_id):
    badge = None
    with model.engine.connect() as conn:
        badge = conn.execute(model.badge.select().where(
            model.badge.c.family_id == badge_util.get_hackathon_badge_family_id(
                hackathon_id, badge_type)
            )).first()
    if badge:
        badge_util.add_badge_if_not_exist(user_id, badge['id'])


def assign_badges_for_hackathon(hackathon_id):
    badges = None
    ret = requests.get('http://127.0.0.1:5000/v1/api/hackathon/{}/leaderboard?\
        limit=3' .format(hackathon_id)).json()
    if len(ret) > 0:
        assign_place_badge(
            hackathon_id,
            badge_type=badge_util.HACK_FIRST,
            user_id=ret[0]['user_id'],
        )
    if len(ret) > 1:
        assign_place_badge(
            hackathon_id,
            badge_type=badge_util.HACK_SECOND,
            user_id=ret[1]['user_id'],
        )
    if len(ret) > 2:
        assign_place_badge(
            hackathon_id,
            badge_type=badge_util.HACK_THIRD,
            user_id=ret[2]['user_id'],
        )


def update_hackathons_badges():
    scheduler.enter(UPDATE_HACKATHON_BADGE_SLEEP, 1, update_hackathons_badges)
    hackathons = requests.get('http://127.0.0.1:5000/v1/api/hackathon?limit=250').json()
    for hackathon in hackathons:
        if hackathon['status'] == "closed":
            assign_badges_for_hackathon(hackathon['hackathon_id'])


scheduler = sched.scheduler(time.time, time.sleep)
scheduler.enter(UPDATE_HACKATHON_BADGE_SLEEP, 1, update_hackathons_badges)
scheduler.run()

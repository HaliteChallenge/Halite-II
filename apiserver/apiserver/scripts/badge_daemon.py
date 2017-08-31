from .. import badge_util, config, model
import requests
import sched
import time
import sqlalchemy
import arrow

UPDATE_HACKATHON_BADGE_SLEEP = 120
UPDATE_TIER_TIME = 60
USERS_PER_QUERY  = 1
scheduler = sched.scheduler(time.time, time.sleep)


def update_hackathons_badges():
    scheduler.enter(UPDATE_HACKATHON_BADGE_SLEEP, 1, update_hackathons_badges)
    hackathons = requests.get(badge_util.URL_HACKATHON).json()
    for hackathon in hackathons:
        if hackathon['status'] == "closed":
            badge_util.assign_badges_for_hackathon(
                hackathon['hackathon_id'],
                hackathon['title']
            )


# Update tier times for a range of users in the ranking;
# returns number of users in that range
def recalculate_tier_time_range(offset, limit):
    ret = requests.get(badge_util.URL_LEADERBOARD.format(offset, limit)).json()
    for user in ret:
        with model.engine.connect() as conn:
            last_user_tier = conn.execute(model.user_tier_history.select()
                .where(model.user_tier_history.c.user_id == user['user_id'])
                    .order_by(model.user_tier_history.c.last_in_tier.desc())
                        .limit(1)).first()

            if last_user_tier and last_user_tier['tier'] == user['tier']:
                # Append to timedelta time spent in tier
                conn.execute(model.user_tier_history.update().where(
                    sqlalchemy.and_(
                        model.user_tier_history.c.user_id == user['user_id'],
                        model.user_tier_history.c.tier == user['tier'],
                    )).values(total_time_in_tier=
                        model.user_tier_history.c.total_time_in_tier +
                        sqlalchemy.sql.func.timediff(
                            sqlalchemy.sql.func.now(),
                            model.user_tier_history.c.last_in_tier,
                )))
 
            # Update last time stamp for tier
            rc = conn.execute(model.user_tier_history.update().where(
                sqlalchemy.and_(
                    model.user_tier_history.c.user_id == user['user_id'],
                    model.user_tier_history.c.tier == user['tier'],
                )).values(last_in_tier=sqlalchemy.sql.func.now())).rowcount
            if rc == 0:
                conn.execute(model.user_tier_history.insert().values(
                    user_id=user['user_id'],
                    tier=user['tier'],
                ))
            badge_util.replace_temp_tier_badge(user['user_id'], user['tier'])
    return len(ret)


def recalculate_tier_time_all():
    offset = 0
    while recalculate_tier_time_range(offset, USERS_PER_QUERY) > 0:
        offset += USERS_PER_QUERY


def update_tier_badges():
    scheduler.enter(UPDATE_TIER_TIME, 1, update_tier_badges)
    recalculate_tier_time_all()
    if config.HALITE_END > arrow.now().datetime:
        badge_util.update_tier_stay_badge()


scheduler.enter(UPDATE_TIER_TIME, 1, update_tier_badges)
scheduler.enter(UPDATE_HACKATHON_BADGE_SLEEP, 1, update_hackathons_badges)
scheduler.run()


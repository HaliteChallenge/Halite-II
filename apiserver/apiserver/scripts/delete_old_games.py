import datetime
import logging
import sys

import sqlalchemy

from .. import config, model, util


def delete_old_games():
    """
    Delete games older than 2 weeks for Bronze-tier players.
    """
    sqlfunc = sqlalchemy.sql.func
    with model.engine.connect() as conn:
        total_users = conn.execute(model.total_ranked_users).first()[0]
        thresholds = util.tier_thresholds(total_users)

        users = conn.execute(
            sqlalchemy.sql.select([
                model.ranked_bots_users.c.user_id,
                model.ranked_bots_users.c.rank,
            ]).where(
                model.ranked_bots_users.c.rank >= thresholds[config.TIER_4_NAME]
            )
        ).fetchall()

        for user in users:
            logging.info("Clearing games for user {}".format(user["user_id"]))
            with conn.begin():
                cutoff_time = sqlfunc.adddate(sqlfunc.now(), -14)
                # Delete all rows older than this in the various game tables
                result = conn.execute(model.games.delete().where(
                    sqlalchemy.sql.exists(
                        model.game_participants.select().where(
                            (model.game_participants.c.user_id == user["user_id"]) &
                            (model.game_participants.c.game_id == model.games.c.id)
                        )
                    ) &
                    (model.games.c.time_played < cutoff_time)
                ))
                logging.info("Deleted {} games".format(result.rowcount))


if __name__ == "__main__":
    if len(sys.argv) > 1:
        handler = logging.handlers.RotatingFileHandler(
            sys.argv[1],
            maxBytes=1024*1024*20,
            backupCount=20)
        handler.setLevel(logging.DEBUG)
        logging.getLogger("").addHandler(handler)

    logging.info("Started job at {}".format(datetime.datetime.utcnow().isoformat()))

    delete_old_games()

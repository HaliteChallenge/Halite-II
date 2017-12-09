"""
Reset all user ratings while saving current rating information by creating a
new bot history entry.
"""

import argparse
import sys

import sqlalchemy

from .. import model

def main(args=sys.argv[1:]):
    parser = argparse.ArgumentParser(description="Reset user ratings.")
    parser.add_argument("--execute", action="store_true")
    cfg = parser.parse_args(args)

    with model.engine.connect() as conn:
        func = sqlalchemy.sql.func
        transaction = conn.begin()
        try:
            total_ranked = model.total_ranked_users.alias("tr")
            history_insert = model.bot_history.insert().from_select([
                    model.bot_history.c.user_id,
                    model.bot_history.c.bot_id,
                    model.bot_history.c.version_number,
                    model.bot_history.c.last_rank,
                    model.bot_history.c.last_score,
                    model.bot_history.c.last_num_players,
                    model.bot_history.c.last_games_played,
                    model.bot_history.c.language,
                ],
                sqlalchemy.sql.select([
                        model.ranked_bots.c.user_id,
                        model.ranked_bots.c.bot_id,
                        model.ranked_bots.c.version_number,
                        sqlalchemy.sql.text("ranked_bots.bot_rank"),
                        model.ranked_bots.c.score,
                        sqlalchemy.sql.text("tr.*"),
                        model.ranked_bots.c.games_played,
                        model.ranked_bots.c.language,
                ]).select_from(model.ranked_bots
                ).select_from(model.total_ranked_users.alias("tr")
                ).where(model.ranked_bots.c.games_played > 0)
            )
            bots_update = model.bots.update().values(
                    version_number=model.bots.c.version_number + 1,
                    games_played=0,
                    mu=25,
                    sigma=8.333,
                    score=0,
                )
            insert_res = conn.execute(history_insert)
            update_res = conn.execute(bots_update)
            max_game_res = conn.execute(sqlalchemy.sql.select([
                    func.max(model.games.c.id)]).select_from(model.games))
            max_game = max_game_res.fetchone()[0]
            print("%d bot history records inserted and %d ratings reset" % (
                insert_res.rowcount, update_res.rowcount))
            print("Reset at game id", max_game)
        except:
            transaction.rollback()
            raise
        else:
            if cfg.execute:
                transaction.commit()
                print("Reset committed.")
            else:
                transaction.rollback()
                print("Reset rolled back, use --execute to commit.")

if __name__ == "__main__":
    main()

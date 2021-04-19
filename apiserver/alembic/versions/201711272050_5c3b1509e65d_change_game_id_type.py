"""Change game id type.

Revision ID: 5c3b1509e65d
Revises: 451d4bb125cb
Create Date: 2017-11-27 20:50:51.717382+00:00

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import mysql


# revision identifiers, used by Alembic.
revision = '5c3b1509e65d'
down_revision = '451d4bb125cb'
branch_labels = None
depends_on = None


def upgrade():
    op.drop_constraint('game_stat_ibfk_1', 'game_stat', type_='foreignkey')
    op.drop_constraint('game_view_stat_ibfk_1', 'game_view_stat',
                       type_='foreignkey')
    op.drop_constraint('game_bot_stat_ibfk_1', 'game_bot_stat',
                       type_='foreignkey')
    op.drop_constraint('game_participant_ibfk_4', 'game_participant',
                       type_='foreignkey')
    op.alter_column('game', 'id',
                    type_=mysql.INTEGER(display_width=9, unsigned=True),
                    existing_nullable=False, existing_autoincrement=True)
    op.alter_column('game_stat', 'game_id',
                    type_=mysql.INTEGER(display_width=9, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_view_stat', 'game_id',
                    type_=mysql.INTEGER(display_width=9, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_bot_stat', 'game_id',
                    type_=mysql.INTEGER(display_width=9, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_participant', 'game_id',
                    type_=mysql.INTEGER(display_width=9, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.create_foreign_key('game_stat_ibfk_1', 'game_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_view_stat_ibfk_1', 'game_view_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_bot_stat_ibfk_1', 'game_bot_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_participant_ibfk_4', 'game_participant', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')


def downgrade():
    op.drop_constraint('game_stat_ibfk_1', 'game_stat', type_='foreignkey')
    op.drop_constraint('game_view_stat_ibfk_1', 'game_view_stat',
                       type_='foreignkey')
    op.drop_constraint('game_bot_stat_ibfk_1', 'game_bot_stat',
                       type_='foreignkey')
    op.drop_constraint('game_participant_ibfk_4', 'game_participant',
                       type_='foreignkey')
    op.alter_column('game', 'id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=True)
    op.alter_column('game_stat', 'game_id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_view_stat', 'game_id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_stat', 'game_id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_bot_stat', 'game_id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.alter_column('game_participant', 'game_id',
                    type_=mysql.MEDIUMINT(display_width=8, unsigned=True),
                    existing_nullable=False, existing_autoincrement=False)
    op.create_foreign_key('game_stat_ibfk_1', 'game_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_view_stat_ibfk_1', 'game_view_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_bot_stat_ibfk_1', 'game_bot_stat', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')
    op.create_foreign_key('game_participant_ibfk_4', 'game_participant', 'game',
                          ['game_id'], ['id'], ondelete='CASCADE')


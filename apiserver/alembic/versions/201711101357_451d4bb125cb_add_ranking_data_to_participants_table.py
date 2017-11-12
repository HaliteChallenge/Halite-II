"""Add ranking data to participants table

Revision ID: 451d4bb125cb
Revises: 49be2190c22d
Create Date: 2017-11-10 13:57:37.807238+00:00

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import mysql


# revision identifiers, used by Alembic.
revision = '451d4bb125cb'
down_revision = '49be2190c22d'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column(
        "game_participant",
        sa.Column('mu', mysql.FLOAT(),
                  nullable=True),
    )
    op.add_column(
        "game_participant",
        sa.Column('sigma',
                  mysql.FLOAT(unsigned=True),
                  nullable=True),
    )
    op.add_column(
        "game_participant",
        sa.Column('leaderboard_rank',
                  mysql.SMALLINT(display_width=5),
                  autoincrement=False,
                  nullable=True),
    )


def downgrade():
    op.drop_column("game_participant", "mu")
    op.drop_column("game_participant", "sigma")
    op.drop_column("game_participant", "leaderboard_rank")

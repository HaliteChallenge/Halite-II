"""Add challenges

Revision ID: 49be2190c22d
Revises: 33de9025cc63
Create Date: 2017-11-08 17:34:20.134831+00:00

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import mysql


# revision identifiers, used by Alembic.
revision = '49be2190c22d'
down_revision = '33de9025cc63'
branch_labels = None
depends_on = None


def upgrade():
    op.create_table(
        "challenge",
        sa.Column("id", sa.Integer, primary_key=True),
        sa.Column("created", sa.DateTime,
                  nullable=False,
                  server_default=sa.sql.func.now()),
        sa.Column("finished", sa.DateTime,
                  nullable=True),
        sa.Column("num_games", sa.Integer,
                  default=0,
                  nullable=False),
        sa.Column("status",
                  sa.Enum("created", "playing_game", "finished"),
                  default="created",
                  nullable=False),
        sa.Column("most_recent_game_task",
                  sa.DateTime,
                  nullable=True),
        sa.Column("issuer",
                  mysql.MEDIUMINT(display_width=8, unsigned=True),
                  nullable=False),
        sa.Column("winner",
                  mysql.MEDIUMINT(display_width=8, unsigned=True),
                  nullable=True),
        sa.ForeignKeyConstraint(['issuer'], ['user.id'],
                                name='challenge_issuer_fk',
                                ondelete='CASCADE'),
        sa.ForeignKeyConstraint(['winner'], ['user.id'],
                                name='challenge_winner_fk',
                                ondelete='CASCADE'),
    )

    op.create_table(
        "challenge_participant",
        sa.Column("challenge_id",
                  sa.Integer,
                  primary_key=True),
        sa.Column("user_id",
                  mysql.MEDIUMINT(display_width=8, unsigned=True),
                  primary_key=True),
        sa.Column("points",
                  sa.Integer(),
                  default=0,
                  nullable=False),
        sa.Column("ships_produced",
                  sa.Integer(),
                  default=0,
                  nullable=False),
        sa.Column("attacks_made",
                  sa.Integer(),
                  default=0,
                  nullable=False),
        sa.ForeignKeyConstraint(['challenge_id'], ['challenge.id'],
                                name='challenge_participant_fk',
                                ondelete='CASCADE'),
        sa.ForeignKeyConstraint(['user_id'], ['user.id'],
                                name='challenge_participant_ibfk_2',
                                ondelete='CASCADE'),
    )

    op.add_column(
        "game",
        sa.Column("challenge_id",
                  sa.Integer,
                  nullable=True),
    )

    op.create_foreign_key('game_challenge_fk',
                          'game',
                          'challenge',
                          ['challenge_id'],
                          ['id'],
                          ondelete='CASCADE')


def downgrade():
    op.drop_constraint("game_challenge_fk", "game", "foreignkey")
    op.drop_column("game", "challenge_id")
    op.drop_table("challenge_participant")
    op.drop_table("challenge")

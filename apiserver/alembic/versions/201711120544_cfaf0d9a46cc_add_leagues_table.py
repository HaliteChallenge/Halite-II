"""Add leagues table.

Revision ID: cfaf0d9a46cc
Revises: e7920cc5568a
Create Date: 2017-11-12 05:44:21.576370+00:00

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import mysql


# revision identifiers, used by Alembic.
revision = 'cfaf0d9a46cc'
down_revision = 'e7920cc5568a'
branch_labels = None
depends_on = None

def upgrade():
    op.create_table(
        'leagues',
        sa.Column('id', mysql.INTEGER(display_width=11), nullable=False,
                  primary_key=True, autoincrement=False),
        sa.Column('category', mysql.VARCHAR(length=45), nullable=False),
        sa.Column('name', mysql.VARCHAR(length=45), nullable=False),
        sa.Column('description', mysql.VARCHAR(length=1024), nullable=False),
        sa.Column('query', mysql.VARCHAR(length=1024), nullable=False),
        mysql_default_charset='utf8',
        mysql_engine='InnoDB'
    )


def downgrade():
    op.drop_table('leagues')

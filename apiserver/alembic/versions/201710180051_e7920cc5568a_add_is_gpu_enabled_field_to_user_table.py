"""Add is_gpu_enabled field to user table.

Revision ID: e7920cc5568a
Revises: 7f0054256cf5
Create Date: 2017-10-18 00:51:17.501319+00:00

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import mysql


# revision identifiers, used by Alembic.
revision = 'e7920cc5568a'
down_revision = '7f0054256cf5'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('user', sa.Column('is_gpu_enabled',
                                    mysql.TINYINT(display_width=1),
                                    nullable=False,
                                    server_default=sa.text("'0'")
                                    )
                 )

def downgrade():
    op.drop_column('user', 'is_gpu_enabled')

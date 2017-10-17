Alembic commands should be run from the directory containing both the alembic
directory and alembic.ini file.

For full alembic documentation see http://alembic.zzzcomputing.com/en/latest/

The file `../sql/schema.sql` can be regenerated with:

>  alembic upgrade head --sql


Creating a new revision to make changes can be started with:

>  alembic revision -m "message on change being made"

then edit the generated file in `alembic/versions/` with the desired database
changes


To migrate the database, first ensure that either the `../apiserver/config.py`
has the `DATABASE_URL` set or `../alembic.ini` has `sqlalchemy.url` set with
database connection details. The `config.py` setting will overide the setting
in `alembic.ini`.

As with any migration it's recommended that a fresh database backup is made
before starting.

You may want to check and record what revision the database is currently on
before starting, this is done with:

>  alembic current

Running the migrations on the database is then just:

>  alembic upgrade head

Depending on the migrations required this may take a few seconds to several
minutes.


If you need to revert to a previous revision:

>  alembic downgrade <revision_id>

Revision id's can be found near the top of the respective migration script in
the `versions` directory. It's also included in the filename directly after
the timestamp.

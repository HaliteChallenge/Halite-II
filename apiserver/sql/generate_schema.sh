#!/bin/sh

# This script will output the current database schema and alembic version.
# It can be useful either when updating schema.sql or to compare the actual
# database against the currently defined schema in schema.sql.
#
# Connection information can be set using environment variables.
#
# Warning: setting the password using the DB_PASS enviroment variable can be a
# security risk as the password will show up in process listings such as 'top'
# or 'ps'.
#
# Example usage:
# DB_HOST=dbserver.local DB_USER=hal ./generate_schema.sh > currentschema.sql

DB_HOST=${DB_HOST:-"localhost"}
DB_PORT=${DB_PORT:-"3306"}
DB_USER=${DB_USER:-"halite"}
DB_NAME=${DB_NAME:-"halite2"}

if [ -z ${DB_PASS+1} ] ; then
    PASS_OPT="--password"
else
    PASS_OPT="--password=$DB_PASS"
fi

mysqldump --host=$DB_HOST --port=$DB_PORT --user=$DB_USER $PASS_OPT --no-data $DB_NAME
mysqldump --host=$DB_HOST --port=$DB_PORT --user=$DB_USER $PASS_OPT $DB_NAME --no-create-info alembic_version

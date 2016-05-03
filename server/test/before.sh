#!/usr/bin/env sh

dropdb $DB_NAME
createdb $DB_NAME -O $DB_USER
psql -U $DB_USER -d $DB_NAME -f sql/latest.sql

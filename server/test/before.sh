#!/usr/bin/env sh

psql -U $DB_USER -c "drop database ${DB_NAME};"
psql -U $DB_USER -c "create database ${DB_NAME};"
psql -U $DB_USER -d $DB_NAME -f sql/latest.sql

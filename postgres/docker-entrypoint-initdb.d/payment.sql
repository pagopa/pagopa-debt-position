\set user `echo "$POSTGRES_USER"`


CREATE DATABASE payment;
GRANT ALL PRIVILEGES ON DATABASE payment TO :user;

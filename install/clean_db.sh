#!/usr/bin/env bash
HOST='localhost'
PORT=3306
USERNAME='goobi'
PASSWORD='goobi'
DATABASE='goobi'

mysql_call() {
  mysql -h $HOST -P $PORT -u $USERNAME --password=$PASSWORD -D $DATABASE -B -N -e "$1"
}

get_tables() {
  mysql_call "SHOW TABLES;"
}

clean_table() {
  mysql_call "DELETE FROM $1;"
}

clean_tables() {
  TABLES=$(get_tables)
  for T in $TABLES
  do
    clean_table $T
  done
}

# Multiple invocations, because every run something else can be deleted and some other things are blocked due to not-yet-deleted reference constraints
clean_tables
clean_tables
clean_tables
clean_tables
clean_tables
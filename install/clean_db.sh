#!/usr/bin/env bash
HOST='localhost'
PORT=3366
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

reset_sequence() {
  mysql_call "ALTER SEQUENCE $1 RESTART WITH 1;"
}

clean_tables() {
  TABLES=$(get_tables | grep -v "flyway_schema_history" | grep -v "_seq")
  for TAB in $TABLES
  do
    clean_table "$TAB"
  done
}

reset_sequences() {
  SEQUENCES=$(get_tables | grep -v "flyway_schema_history" | grep "_seq")
  for SEQ in $SEQUENCES
  do
    reset_sequence "$SEQ"
  done
}

# Multiple invocations, because every run something else can be deleted and some other things are blocked due to not-yet-deleted reference constraints
clean_table field_translation
clean_table field_value
clean_table field_instance
clean_table vocabulary_record
clean_table vocabulary
clean_table selectable_value
clean_table field_definition
clean_table translation_definition
clean_table vocabulary_schema
clean_table field_type
clean_table language
clean_tables
clean_tables
reset_sequences
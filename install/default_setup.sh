#!/usr/bin/env bash
HOST='localhost:8080/api/v1'

curl_call() {
  curl --location "$HOST/$1" --header 'Content-Type: application/json' --data "$2"
}

create_language() {
  curl_call "languages" "{\"abbreviation\":\"$1\", \"name\":\"$2\"}"
}

create_type() {
  if [ -n "$3" ]
  then
    if [ -n "$2" ]
    then
      curl_call "types" "{\"name\":\"$1\",\"validation\":\"$2\",\"selectableValues\":[$3]}"
    else
      curl_call "types" "{\"name\":\"$1\",\"selectableValues\":[$3]}"
    fi
  else
    if [ -n "$2" ]
    then
      echo "{\"name\":\"$1\",\"validation\":\"$2\"}"
      curl_call "types" "{\"name\":\"$1\",\"validation\":\"$2\"}"
    else
      curl_call "types" "{\"name\":\"$1\"}"
    fi
  fi
}

create_language "none" "non-translated value"
create_language "eng" "English"
create_type "Anything"
create_type "Boolean" "" "\"true\",\"false\""
create_type "Number" "\\\\d+"
create_type "Word" "\\\\w+"
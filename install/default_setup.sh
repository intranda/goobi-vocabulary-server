#!/usr/bin/env bash
HOST='localhost:8081/api/v1'

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

create_language "eng" "English"
create_language "ger" "Deutsch"
create_language "fre" "Français"
create_type "Anything"
curl_call "types" "{\"name\":\"Any Text\",\"large\":true}"
create_type "Boolean" "" "\"true\",\"false\""
create_type "Number" "\\\\d+"
create_type "Word" "\\\\w+"
create_type "skos:prefLabel"
create_type "skos:altLabel"
create_type "skos:definition"
create_type "skos:editorialNote"
create_type "skos:related" "\\\\d+"
create_type "skos:closeMatch" "https?.*"
create_type "skos:exactMatch" "https?.*"
create_type "dct:title"
create_type "dct:creator"
create_type "dct:created" "\\\\d{4}\\\\-\\\\d{2}\\\\-\\\\d{2}"
create_type "dct:license" "https?.*"

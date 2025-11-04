---
title: Snippets
published: true
keywords:
    - Vocabulary Server
---
Below are some snippets that illustrate how the vocabulary server can be put into operation using the example of a Goobi-to-go instance:

## Test for content in the vocabulary server

```bash
curl "http://localhost:8081/api/v1/types" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/schemas" --header "Authorization: Bearer secret" | jq
curl "http://localhost:8081/api/v1/vocabularies" --header "Authorization: Bearer secret" | jq
```

## Downloading the installation script

```bash
wget https://github.com/intranda/goobi-vocabulary-server/releases/latest/download/vocabulary-init-script.zip
unzip vocabulary-init-script.zip
```

## Example of creating minimal types

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh minimal
```

## Example of creating vocabularies for books

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh books
```


## Example of creating vocabularies for collections

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh collections
```

## Example of creating hierarchical vocabularies for collections

```bash
HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh hierarchical_collections
```

## Complete CURL calls
For every sample, you can display the full CURL calls the following way:
```bash
VERBOSE=1 HOST=localhost PORT=8081 TOKEN=secret vocabulary-init-script/install.sh collections
```

For the collections sample it looks like this:
```bash
curl --location "localhost:8081/api/v1/types" --header 'Content-Type: application/json' --header "Authorization: Bearer secret" --fail --data "{ \"name\": \"Anything\" }"
curl --location "localhost:8081/api/v1/types" --header 'Content-Type: application/json' --header "Authorization: Bearer secret" --fail --data "{ \"name\": \"Large Text\", \"large\": true }"
curl --location "localhost:8081/api/v1/schemas" --header 'Content-Type: application/json' --header "Authorization: Bearer secret" --fail --data "{ \"definitions\": [ { \"name\": \"Name\", \"typeId\": 1, \"required\": true, \"unique\": true, \"mainEntry\": true, \"titleField\": false }, { \"name\": \"Label\", \"typeId\": 1, \"required\": true, \"unique\": false, \"mainEntry\": false, \"titleField\": true }, { \"name\": \"Description\", \"typeId\": 2, \"required\": false, \"unique\": false, \"mainEntry\": false, \"titleField\": false, \"multiValued\": false } ] }"
curl --location "localhost:8081/api/v1/vocabularies" --header 'Content-Type: application/json' --header "Authorization: Bearer secret" --fail --data "{ \"schemaId\": 1, \"name\": \"Sammlungen\", \"description\": \"Liste alle verfügbaren Sammlungen\" }"
curl --location "localhost:8081/api/v1/vocabularies/1/import/csv" --header "Authorization: Bearer secret" --fail --form "file=@\"/home/dominick/workspace/goobi-workflow/vocabulary-server/init-script/samples/collections/4_records/Collections.csv\""
```

Or for the more complex books sample:
```bash
# === Languages ===
curl --location "http://localhost:8081/api/v1/languages" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"abbreviation": "ger", "name": "Deutsch"}'

curl --location "http://localhost:8081/api/v1/languages" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"abbreviation": "eng", "name": "English"}'

curl --location "http://localhost:8081/api/v1/languages" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"abbreviation": "fre", "name": "Français"}'


# === Types ===
curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Anything"}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Datum", "validation": "\\\\d{2}\\\\.\\\\d{2}\\\\.\\\\d{4}"}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Hex RGB", "validation": "^#[0-9A-Fa-f]{6}$"}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "ISBN-13", "validation": "\\\\d{3}-\\\\d{1}-\\\\d{2}-\\\\d{6}-\\\\d{1}"}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Large Text", "large": true}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Number", "validation": "\\\\d+"}'

curl --location "http://localhost:8081/api/v1/types" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"name": "Ja/Nein", "selectableValues": ["Ja", "Nein"]}'


# === Schemas ===
curl --location "http://localhost:8081/api/v1/schemas" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{
    "definitions": [
      {
        "name": "Name",
        "typeId": 1,
        "required": true,
        "unique": true,
        "mainEntry": true,
        "titleField": true,
        "translationDefinitions": [
          { "language": "ger", "fallback": true, "required": true },
          { "language": "eng", "fallback": false, "required": false },
          { "language": "fre", "fallback": false, "required": false }
        ]
      },
      {
        "name": "Farbcode",
        "typeId": 3,
        "required": false,
        "unique": true,
        "mainEntry": false,
        "titleField": false,
        "multiValued": false
      }
    ]
  }'

curl --location "http://localhost:8081/api/v1/schemas" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{
    "definitions": [
      {
        "name": "Name",
        "typeId": 1,
        "required": true,
        "unique": true,
        "mainEntry": true,
        "titleField": true,
        "translationDefinitions": [
          { "language": "ger", "fallback": true, "required": true },
          { "language": "eng", "fallback": false, "required": false },
          { "language": "fre", "fallback": false, "required": false }
        ]
      }
    ]
  }'


# === Vocabularies ===
curl --location "http://localhost:8081/api/v1/vocabularies" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"schemaId": 2, "name": "Autoren", "description": "Liste aller bekannter Autoren."}'

curl --location "http://localhost:8081/api/v1/vocabularies" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"schemaId": 1, "name": "Farben"}'

curl --location "http://localhost:8081/api/v1/vocabularies" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"schemaId": 2, "name": "Standorte", "description": "Liste aller Standorte an denen Bücher verwaltet werden."}'


# === Books-Schema ===
curl --location "http://localhost:8081/api/v1/schemas" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{
    "definitions": [
      { "name": "Buch-ID", "typeId": 6, "required": true, "unique": true, "mainEntry": false, "titleField": false },
      {
        "name": "Name",
        "typeId": 1,
        "required": true,
        "unique": true,
        "mainEntry": true,
        "titleField": true,
        "translationDefinitions": [
          { "language": "ger", "fallback": true, "required": true },
          { "language": "eng", "fallback": false, "required": false },
          { "language": "fre", "fallback": false, "required": false }
        ]
      },
      { "name": "ISBN", "typeId": 4, "required": true, "unique": true, "mainEntry": false, "titleField": false },
      { "name": "Veröffentlichung", "typeId": 2, "required": false, "unique": false, "mainEntry": false, "titleField": false },
      { "name": "Autoren", "referenceVocabularyId": 1, "required": false, "unique": false, "mainEntry": false, "titleField": false, "multiValued": true },
      { "name": "Buchfarbe", "referenceVocabularyId": 2, "required": true, "unique": false, "mainEntry": false, "titleField": true, "multiValued": false },
      { "name": "Standort", "referenceVocabularyId": 3, "required": true, "unique": false, "mainEntry": false, "titleField": true, "multiValued": false },
      { "name": "Beschreibung", "typeId": 5, "required": false, "unique": false, "mainEntry": false, "titleField": false, "multiValued": false },
      { "name": "Ausgeliehen", "typeId": 7, "required": true, "unique": false, "mainEntry": false, "titleField": false, "multiValued": false }
    ]
  }'


# === Books-Vocabulary ===
curl --location "http://localhost:8081/api/v1/vocabularies" \
  --header "Content-Type: application/json" \
  --header "Authorization: Bearer secret" \
  --fail \
  --data '{"schemaId": 3, "name": "Bücher", "description": "Buchdatenbank."}'


# === CSV-Imports ===
curl --location "http://localhost:8081/api/v1/vocabularies/1/import/csv" \
  --header "Authorization: Bearer secret" \
  --fail \
  --form "file=@/home/dominick/workspace/goobi-workflow/vocabulary-server/init-script/samples/books/7_records/Authors.csv"

curl --location "http://localhost:8081/api/v1/vocabularies/2/import/csv" \
  --header "Authorization: Bearer secret" \
  --fail \
  --form "file=@/home/dominick/workspace/goobi-workflow/vocabulary-server/init-script/samples/books/7_records/Colors.csv"

curl --location "http://localhost:8081/api/v1/vocabularies/3/import/csv" \
  --header "Authorization: Bearer secret" \
  --fail \
  --form "file=@/home/dominick/workspace/goobi-workflow/vocabulary-server/init-script/samples/books/7_records/Locations.csv"
```

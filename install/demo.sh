#!/usr/bin/env bash
METADATA_ID=$(curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
    "definitions": [
        {
            "name": "Title",
            "typeId": 12,
            "required": true,
            "unique": false,
            "mainEntry": true,
            "titleField": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Author",
            "typeId": 13,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Creation Date",
            "typeId": 14,
            "required": true,
            "unique": false,
            "mainEntry": false,
            "titleField": false
        },
        {
            "name": "License",
            "typeId": 15,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        }
    ],
    "hierarchicalRecords": false,
    "singleRootElement": true
}' | jq ".id")
SKOS_ID=$(curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
    "definitions": [
        {
            "name": "Label",
            "typeId": 6,
            "required": true,
            "unique": false,
            "mainEntry": true,
            "titleField": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Alternative Label",
            "typeId": 7,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Definition",
            "typeId": 8,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Editorial Note",
            "typeId": 9,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                },
                {
                    "language": "fre",
                    "fallback": false,
                    "required": false
                }
            ]
        },
        {
            "name": "Related",
            "typeId": 1,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        },
        {
            "name": "Close Match",
            "typeId": 10,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        },
        {
            "name": "Exact Match",
            "typeId": 11,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        }
    ],
    "hierarchicalRecords": true
}' | jq ".id")
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data "{
    \"schemaId\": $SKOS_ID,
    \"metadataSchemaId\": $METADATA_ID,
    \"name\": \"SKOS\",
    \"description\": \"SKOS example vocabulary.\"
}"
RATING_ID=$(curl --location 'localhost:8081/api/v1/types' \
--header 'Content-Type: application/json' \
--data '{
    "name": "Rating",
    "validation": "\\d+",
    "selectableValues": [
        "1", "2", "3", "4", "5"
    ]
}' | jq ".id")
MOVIE_SCHEMA_ID=$(curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data "{
    \"definitions\": [
        {
            \"name\": \"Name\",
            \"typeId\": 5,
            \"required\": true,
            \"unique\": true,
            \"mainEntry\": true,
            \"titleField\": true
        },
        {
            \"name\": \"Description\",
            \"typeId\": 1,
            \"required\": false,
            \"unique\": false,
            \"mainEntry\": false,
            \"titleField\": false,
            \"translationDefinitions\": [
                {
                    \"language\": \"eng\",
                    \"fallback\": true,
                    \"required\": true
                },
                {
                    \"language\": \"ger\",
                    \"fallback\": false,
                    \"required\": true
                }
            ]
        },
        {
            \"name\": \"Rating\",
            \"typeId\": $RATING_ID,
            \"required\": false,
            \"unique\": false,
            \"mainEntry\": false,
            \"titleField\": false
        }
    ],
    \"hierarchicalRecords\": true
}" | jq ".id")
MOVIE_VOCAB_ID=$(curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data "{
    \"schemaId\": $MOVIE_SCHEMA_ID,
    \"name\": \"MovieDB\",
    \"description\": \"Personal movie database.\"
}" | jq ".id")
HOBBY_SCHEMA_ID=$(curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
    "definitions": [
        {
            "name": "Description",
            "typeId": 1,
            "required": true,
            "unique": true,
            "mainEntry": true,
            "titleField": true,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": false
                }
            ]
        }
    ],
    "hierarchicalRecords": false
}' | jq ".id")
HOBBY_VOCAB_ID=$(curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data "{
    \"schemaId\": $HOBBY_SCHEMA_ID,
    \"name\": \"Hobbies\",
    \"description\": \"Collection of typical hobbies.\"
}" | jq ".id")
FACEBOOK_ID=$(curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data "{
   \"definitions\": [
         {
            \"name\": \"Full Name\",
            \"typeId\": 1,
            \"required\": true,
            \"unique\": true,
            \"mainEntry\": true,
            \"titleField\": true
        },
        {
            \"name\": \"Favorite Movie\",
            \"referenceVocabularyId\": $MOVIE_VOCAB_ID,
            \"required\": true,
            \"unique\": false,
            \"mainEntry\": false,
            \"titleField\": true
        },
        {
            \"name\": \"Hobbies\",
            \"referenceVocabularyId\": $HOBBY_VOCAB_ID,
            \"required\": false,
            \"unique\": false,
            \"mainEntry\": false,
            \"titleField\": false,
            \"multiValued\": true
        }
    ]
}" | jq ".id")
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data "{
    \"schemaId\": $FACEBOOK_ID,
    \"name\": \"Facebook\",
    \"description\": \"My best friends with their favorite movies.\"
}"

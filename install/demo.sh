#!/usr/bin/env bash
curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
    "definitions": [
        {
            "name": "Title",
            "typeId": 13,
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
            "typeId": 14,
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
            "typeId": 15,
            "required": true,
            "unique": false,
            "mainEntry": false,
            "titleField": false
        },
        {
            "name": "License",
            "typeId": 16,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        }
    ],
    "hierarchicalRecords": false,
    "singleRootElement": true
}'
curl --location 'localhost:8081/api/v1/schemas' \
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
            "name": "Close Match",
            "typeId": 11,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        },
        {
            "name": "Exact Match",
            "typeId": 12,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        }
    ],
    "hierarchicalRecords": true
}'
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data '{
    "schemaId": 2,
    "metadataSchemaId": 1,
    "name": "SKOS",
    "description": "SKOS example vocabulary."
}'
curl --location 'localhost:8081/api/v1/types' \
--header 'Content-Type: application/json' \
--data '{
    "name": "Rating",
    "validation": "\\d+",
    "selectableValues": [
        "1", "2", "3", "4", "5"
    ]
}'
curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
    "definitions": [
        {
            "name": "Name",
            "typeId": 5,
            "required": true,
            "unique": true,
            "mainEntry": true,
            "titleField": true
        },
        {
            "name": "Description",
            "typeId": 1,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "translationDefinitions": [
                {
                    "language": "eng",
                    "fallback": true,
                    "required": true
                },
                {
                    "language": "ger",
                    "fallback": false,
                    "required": true
                }
            ]
        },
        {
            "name": "Rating",
            "typeId": 17,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false
        }
    ],
    "hierarchicalRecords": true
}'
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data '{
    "schemaId": 3,
    "name": "MovieDB",
    "description": "Personal movie database."
}'
curl --location 'localhost:8081/api/v1/schemas' \
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
}'
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data '{
    "schemaId": 4,
    "name": "Hobbies",
    "description": "Collection of typical hobbies."
}'
curl --location 'localhost:8081/api/v1/schemas' \
--header 'Content-Type: application/json' \
--data '{
   "definitions": [
         {
            "name": "Full Name",
            "typeId": 1,
            "required": true,
            "unique": true,
            "mainEntry": true,
            "titleField": true
        },
        {
            "name": "Favorite Movie",
            "referenceVocabularyId": 2,
            "required": true,
            "unique": false,
            "mainEntry": false,
            "titleField": true
        },
        {
            "name": "Hobbies",
            "referenceVocabularyId": 3,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false,
            "multiValued": true
        }
    ]
}'
curl --location 'localhost:8081/api/v1/vocabularies' \
--header 'Content-Type: application/json' \
--data '{
    "schemaId": 5,
    "name": "Facebook",
    "description": "My best friends with their favorite movies."
}'

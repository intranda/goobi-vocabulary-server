#!/usr/bin/env bash
curl --location 'localhost:8081/api/v1/schemas' \
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
    "hierarchicalRecords": true,
    "singleRootElement": true
}'
curl --location 'localhost:8081/api/v1/schemas' \
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
    "singleRootElement": false
}'
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
    "hierarchicalRecords": true,
    "singleRootElement": false
}'

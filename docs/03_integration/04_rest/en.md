---
title: Queries via the REST API
published: true
keywords:
    - Vocabulary Server
---
Besides Goobi workflow, other applications can also read the vocabularies through the REST API of the vocabulary server, for example an ordering system that needs to show locations. All calls start with `/api/v1` and require the vocabulary server's token in the `Authorization` header.

The examples on this page assume a vocabulary server at `http://localhost:8081` with the token `secret-token`, and the vocabulary `Locations` with the ID `1`. Adapt address, token and IDs to your installation. The appended `| python3 -m json.tool` only formats the response for reading and can be left out.

## Finding the vocabulary and schema
Find the vocabulary by its name. The response contains the vocabulary's `id` and the `schemaId` of its schema:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/by-name/Locations" | python3 -m json.tool
```

List all vocabularies:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies" | python3 -m json.tool
```

Retrieve the schema. The `id` of each field definition is needed for searching:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/schemas/1" | python3 -m json.tool
```

## Retrieving records
Retrieve the records of a vocabulary:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/1/records" | python3 -m json.tool
```

For hierarchical vocabularies, this call only returns the records of the top level. Child records are listed as IDs in the field `children`, the parent record in the field `parentId`.

Retrieve a single record:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/records/27" | python3 -m json.tool
```

Retrieve the same record as XML:

```bash
curl -s -H "Authorization: Bearer secret-token" -H "Accept: application/xml" "http://localhost:8081/api/v1/records/27"
```

## Searching records
The parameter `search` finds records by the content of a field. The form is `fieldId:value`, where the field ID comes from the schema. The search finds all records whose field contains the value and, for hierarchical vocabularies, searches the whole tree.

Find all compartments (in the example, the field `Level` has the ID `3`):

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=3:Compartment&size=100" | python3 -m json.tool
```

Find all records below stack room 2 (the field `Code` has the ID `1`):

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=1:HG-M2&size=100" | python3 -m json.tool
```

Spaces in the search value are written as `%20`:

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=2:Compartment%204" | python3 -m json.tool
```

## Paging
Lists are delivered in pages, by default with 20 entries per page. `size` sets the page size, `page` the page (starting at `0`). The total counts are in the `page` section of the response.

```bash
curl -s -H "Authorization: Bearer secret-token" "http://localhost:8081/api/v1/vocabularies/1/records?search=3:Compartment&size=5&page=1" | python3 -m json.tool
```

## Structure of the response
The values of a record are in `fields`. `definitionId` states which field of the schema a value belongs to. Each value can have translations in several languages. For fields that cannot be translated, `language` is empty. Shortened, a record looks like this:

```json
{
    "id": 27,
    "parentId": 23,
    "vocabularyId": 1,
    "fields": [
        {
            "definitionId": 1,
            "values": [ { "translations": [ { "language": null, "value": "HG-M2-R01-RG01-F4" } ] } ]
        },
        {
            "definitionId": 2,
            "values": [ { "translations": [ { "language": null, "value": "Compartment 4" } ] } ]
        }
    ]
}
```

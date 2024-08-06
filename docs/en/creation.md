# Vocabulary Creation Guide
Vocabularies are a very powerful tool to store arbitrary structured data.
Therefore, the process of creating vocabularies isn't trivial.
In this documentation, we will cover all aspects of vocabularies that are required to properly use them.
You can follow this documentation from top to bottom.

Each of the involved elements needs to be created by API calls to the vocabulary server. 
You can make these API calls with `CURL` for example. 
All calls follow mostly the same pattern.
You need to set the `BASE_PART` to `localhost:8081/api/v1`, change your hostname and port accordingly, leave the `/api/v1` untouched.
Only change the `API_ENDPOINT` and `DATA` parts depending on the type of query you perform:

```bash
curl -s --location "http://BASE_PART/API_ENDPOINT" --header 'Content-Type: application/json' --data "DATA"
```

If you receive any errors, the error information is returned as complex JSON objects.
You can pipe the result into `jq` to extract the relevant error message as follows:
```bash
curl_call  | jq ".message"
```

If the call is successful, you will receive the API element as a response.
Most vocabulary elements reference each other via ID.
To get the generated ID for an inserted element, you can also use `jq`:
```bash
curl_call | jq ".id"
```

## Overview
We would like to start with a simplified overview to get you started.

*Vocabularies* are just a collection of data entries, so-called *vocabulary records*.
All vocabulary records in a vocabulary conform to the same *vocabulary schema*, which is part of the vocabulary.
A vocabulary schema specifies the possible *fields* a vocabulary record can have.
The fields of a vocabulary record can have one or many values, possibly with multiple translations.
At the lowest end, these translatable values represent data of some *field types*.

## Languages
If you want to work with languages and translatable values in your vocabulary, you need to define these languages first.
Each language consists of the following information:
- A unique `name`
- A language `abbreviation`

You can create languages with calls to the `languages` API endpoint:
```json
{
    "abbreviation": "eng",
    "name": "English"
}
```

## Field Types
At the lowest end of a vocabulary are the values.
Each value is of a specified type.
These types are not fixed and can be freely defined by the end-user to a certain extent.
A field type consists of the following information:
- A unique `name`
- An optional regular expression for `validation`
- The switch to contain `large` data
- An optional set of `selectableValues`

You can create types by sending API requests to the `types` API endpoint, with the following `DATA` examples.

A normal field type that can contain any kind of data could be specified like this:
```json
{
    "name": "Anything"
}
```

A numerical type that can hold values from `1` to `5` (e. g. a product rating) can be defined like this:
```json
{
    "name": "Rating",
    "selectableValues": [
        "1", "2", "3", "4", "5"
    ]
}
```

You can achieve the exactly same type by solving this by a regular expression validation:
```json
{                                                                                                                                                    
    "name": "Rating",
    "validation": "[1-5]"
}
```

If you missed to save the IDs, you can call the following command to retrieve all existing field types with their IDs:
```bash
curl -s --location "http://BASE_PART/types" --header 'Content-Type: application/json' | jq '._embedded .fieldTypeList .[] | "\(.id) \(.name)"'
```

*Note the missing `--data` parameter here.*

## Vocabulary Schemas
A vocabulary schema defines the structure of a vocabulary, i. e. all available fields each vocabulary record can have.
A vocabulary schema consists of the following information:
- A list of field `definitions`
- A switch to enable `hierarchicalRecords`
- A switch to restrict the vocabulary to a `singleRootElement`

You can create vocabulary schemas with calls to the `schemas` API endpoint.

Let's first discuss the last two pieces before diving into the field definitions.
Normally, vocabularies contain just a potentially long list of vocabulary records.
It is possible, however, to structure vocabulary records hierarchically (e. g. you want to have an entry representing a "Material" with a child element "Iron").
If you want to enable this hierarchy feature, set it to:
```json
"hierarchicalRecords": true
```

If you want to restrict your vocabulary to only allow a single root element (mostly useful in hierarchical vocabularies because you would otherwise allow only one single entry at all), set it to:
```json
"singleRootElement": false
```

If you don't set any of those values, they will be disabled by default.

Let's now focus on the field definitions.
Each field definition consists of the following information:
- A unique `name` among all other definitions
- Either a `typeId` referencing an existing field type (see previous section) or a `referenceVocabularyId` referencing an existing vocabulary
- A switch to set the field value to be `required`
- A switch to set the field value to be `unique` among all vocabulary records in the vocabulary
- A switch to set the field value to be the `mainEntry` of the vocabulary record
- A switch to set the field value to be a `titleField` of the vocabulary record
- A switch to set the field to be `multiValued`
- An optional list of `translationDefinitions`

Vocabularies represent structured data, i. e. all data entries (vocabulary records) have the same fields.
The `name` of the field can be anything to describe the data that is stored in this field.
You can think of the `name` as the column name, if you visualized your vocabulary data in a table.

Vocabulary records can store either values that match an existing field type by specifying its `typeId` or reference other vocabulary records by specifying the corresponding ID of the vocabulary as the `referenceVocabularyId`.
The latter could be useful if you have a vocabulary containing hobbies for example with the vocabulary records "Painting", "Sport", "Sightseeing".
Now imagine you have another vocabulary that represents famous artists and you want to have a field that corresponds to the artist's hobbies.
Instead of copying the hobbies for each artist and thereby duplicating data, you could set this field to reference the "Hobby" vocabulary.

Each vocabulary record that belongs to this vocabulary schema can only define fields that are specified in the `definitions`, not more.
If a field definition is set to be `required`, each vocabulary record must contain a field value or the API will report an error.
Otherwise, the field is optional.

If a field definition is set to be `unique`, any vocabulary records value in this field needs to be unique across the whole vocabulary.
Inserting or changing a vocabulary record to contain a unique value that is already present results in an error.

If a field definition is set to be the `mainEntry`, this fields value will be primarily used when the vocabulary record is referenced anywhere.
Each schema needs to define exactly one `mainEntry` field.

If a field definition is set to be the `titleField`, this field is shown in the tabular preview of the vocabulary records in Goobi workflow.

If a field definition is set to be `multiValued`, the field can hold multiple values.
In the case of the previously introduced artist vocabulary, each artist could have multiple hobbies.
The birthday of an artist, however, should not be `multiValued`.

If you want to store multiple languages in the vocabulary, i. e. translations to the stored values, you can define `translationDefinitions`.
If you don't provide this attribute, the field is treated as a non-translatable value (e. g. a number or a date).
If you want to provide translations, you need to provide translation definitions of the following form:
```json
{
  "language": "eng",
  "fallback": true,
  "required": true
}
```

Exactly one of the language translations needs to be set to `fallback`.
The fallback language needs to be set `required`.
Every non-required language can be left empty when providing values later on.

**Example**
A field is defined to be translatable into `eng` and `ger` with `eng` being the required fallback and `ger` being optional.
In a vocabulary record, only the English translation is given and this vocabulary record is later requested in German.
In this case, because English is the fallback language, the English value will be returned.

**Example**
Let's give an example of a whole vocabulary schema definition:
```json
{
    "definitions": [
        {
            "name": "Name",
            "typeId": 1,
            "required": true,
            "unique": true,
            "mainEntry": true,
            "titleField": true
        },
        {
            "name": "Description",
            "typeId": 2,
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
                    "required": false
                }
            ]
        },
        {
            "name": "Rating",
            "typeId": 3,
            "required": false,
            "unique": false,
            "mainEntry": false,
            "titleField": false
        }
    ]
}
```

This vocabulary schema consists of three fields "Name", "Description", "Rating".
It can be used to describe movies, articles in a shop or favorite toys.
The "Name" field is set to be the required main entry.
Its type ID `1` should correspond to the "Anything" type we created earlier.
The "Description" field should contain a longer description of the entry.
It has a required English translation and an additional optional German translation.
Its type ID `2` could correspond to a field type that is set to `large`.
The "Rating" field should contain a numeric rating value and correspond to the "Rating" type we have created earlier.
This vocabulary schema is not hierarchical and can contain any number of records.

Please check out the [example vocabulary setup script](../install/demo.sh) to see some more complex examples.
These examples require some field types to be present, you can [install these default field types with another provided script](../install/default_setup.sh).

With vocabulary schemas created, we can finally create vocabularies.

## Vocabularies
In order to create vocabularies, you first need to create its vocabulary schemas.
Vocabulary schemas can be reused by as many vocabularies as you wish (e. g. you can use the vocabulary schema from the previous section for the two vocabularies "Movies" and "Amazon Wishlish").

Each vocabulary consists of the following information:
- A unique `name`
- An optional `description`
- A required `schemaId`
- An optional `metadataSchemaId`

You can create vocabulary schemas with calls to the `vocabularies` API endpoint.

The `name` must be unique among all vocabularies, it is often used to reference a vocabulary in other Goobi workflow plugins.
The optional `description` can be any kind of text to describe the vocabulary or its data.
The `schemaId` must reference an existing vocabulary schema, its purpose should be obvious now.
All vocabulary records in this vocabulary must conform to the vocabulary schema with ID `schemaID`.

If you want to store additional metadata in the vocabulary that is not related to any vocabulary record, you can specify a metadata schema with the `metadataSchemaId` attribute.
You could for example create an additional schema for the fields "Creation Date" and "Authors" to store these additional information alongside the vocabulary.
This metadata schema is created like any other schema.
In order to use this schema as a metadata schema in a vocabulary, the vocabulary schema must be non-hierarchical and restricted to a single root element.
This is required, because there can be only one entry of metadata per vocabulary.
This doesn't mean, however, that the fields of the metadata schema can't be multi-valued!

**Example**
Consider the case that you created a metadata schema with the single-valued field "Creation Date" and the multi-valued field "Authors".
If you set this vocabulary schema as the vocabularies metadata schema, the metadata can hold one creation date and arbitrary many authors.

**Example**
Let's create a movie database with the previously created vocabulary schema with ID `1`:
```json
{
    "schemaId": 1,
    "name": "MovieDB",
    "description": "Personal movie database."
}
```

After you have created all vocabularies, you can connect Goobi workflow to the vocabulary server and start creating or editing vocabulary records in Goobi itself.
You can also import and export vocabulary records from and into various formats.

**RDF**
The vocabulary management server also supports special types for RDF compliant data.
For this to work, you need to have RDF compatible types created and create a schema that only consists of these types.
The resulting vocabulary will additionally be exportable into RDF formats like RDF/XML or Turtle.

If you used the [language and field type setup script](../install/default_setup.sh) and the [demo vocabulary setup script](../install/demo.sh), you will have an example of such a RDF vocabulary ready to go.

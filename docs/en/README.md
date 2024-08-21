# Documentation
This document describes the new vocabulary server. Until version 24.06, vocabularies were part of Goobi Workflow and saved in Goobi's database. Since version 24.07, everything related to vocabularies moved to a stand-alone application, the vocabulary server. The vocabulary server requires its own database to store all its data and provides access to the vocabularies and records through a REST API. Goobi Workflow has been updated to use the new vocabulary server instead of its own, embedded vocabularies. If desired, the vocabulary server could be publicly available. If you already used vocabularies before, check out the migration guide in this documentation to transfer your data to the new vocabulary server.

## Setup
Before you can start using the vocabulary server, follow the [setup instructions](setup.md).

## Vocabulary Creation
Vocabularies and vocabulary schemas are a complex topic on their own, therefore the [documentation of the creation of vocabularies, schemas and field types](creation.md) is covered separately.

## Migration of existing Vocabularies
If you have existing vocabulary data from earlier versions of Goobi that you want to migrate, please follow the [data migration documentation](migration.md).

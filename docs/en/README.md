# Documentation
This document describes everything important regarding the new vocabulary server. Earlier, vocabularies were part of Goobi Workflow and saved in the `goobi` database. Now, everything related to vocabularies moved to a new stand-alone application, the vocabulary server. The vocabulary server requires its own database to store all its data and provides access to the vocabularies and records through a REST API. Goobi Workflow has been updated to use the new vocabulary server instead of its own, embedded vocabularies. If desired, the vocabulary server could be publicly available in contrast to the Goobi Workflow instance. If you already used vocabularies before, check out the migration guide in this documentation to transfer your data to the new vocabulary server.

## Setup
Before you can start using the new vocabulary server, follow the [setup instructions](setup.md).

## Vocabulary Creation
Vocabularies and vocabulary schemas are a complex topic on their own, therefore the [documentation of the creation of vocabularies, schemas and field types](creation.md) is covered separately.

## Migration of existing Vocabularies
If you have existing vocabulary data from earlier versions of Goobi that you want to migrate, please follow the [data migration documentation](migration.md).

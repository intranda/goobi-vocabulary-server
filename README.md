# Documentation
This document describes everything important regarding the new vocabulary server. Earlier, vocabularies were part of Goobi Workflow and saved in the `goobi` database. Now, everything related to vocabularies moved to a new stand-alone application, the vocabulary server. The vocabulary server requires its own database to store all its data and provides access to the vocabularies and records through a REST API. Goobi Workflow has been updated to use the new vocabulary server instead of its own, embedded vocabularies. If desired, the vocabulary server could be publicly available in contrast to the Goobi Workflow instance. If you already used vocabularies before, check out the migration guide in this documentation to transfer your data to the new vocabulary server.

## Download and Installation
- Download [Latest Build](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/target/) of vocabulary server.
- Download [Configuration File](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/src/main/resources/application.properties) of the vocabulary server.
- Adapt configuration file properly and remove unmodified lines.
    - Database credentials and database name.
    - Base URL and port.
- Vocabulary server JAR and configuration file need to be placed in the same directory, it doesn't matter where.

## Start and Service
- Navigate to the installation path.
- Run `java -jar vocabulary-server-VERSION.jar`.
- If startup succeeds, you will see a line like this after a few seconds:
```bash
Started VocabularyServerApplication in 4.244 seconds (process running for 4.581)
```
- **TODO** Create a systemd service for the vocabulary server. The application should be able to correctly shutdown on SIGTERM.

## Initial Setup
- For proper operation, the vocabulary server needs some initial data.
- Download the [Initial Data Script](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/install/default_setup.sh) and change the `HOST` variable at the top according to the vocabulary server configuration, leave the `/api/v1` suffix unchanged.
- Run the script

## Vocabulary Migration
- The vocabulary server doesn't contain any vocabularies yet.
- In order to migrate existing vocabularies to the new vocabulary server, follow the [documentation of the vocabulary migration tool](migration/README.md).
   
*Data cleanup* When the data migration is done and successful and you are sure you will never need the old data anymore, you can manually remove all vocabulary tables from the `goobi` database of your Goobi instance:
- `vocabulary`
- `vocabulary_record`
- `vocabulary_record_data`
- `vocabulary_structure`

We suggest keeping this data for some time in case anything unexpected happens.

## Security
- You can set-up Apache url restrictions in order to secure the vocabulary server from unauthorized access.
- **TODO** Please find out what and how to do it in detail.

## Verification
- For all commands, change host and port accordingly.
- After the initial setup, check that types were created successfully:
```bash
curl http://localhost:8081/api/v1/types | jq -r '._embedded.fieldTypeList[] .name'
```
- The result should look like:
```bash
Anything
Boolean
Number
Word
skos:prefLabel
skos:altLabel
skos:definition
skos:editorialNote
skos:related
skos:closeMatch
skos:exactMatch
```

- Check that all vocabularies have been migrated:
```bash
curl http://localhost:8081/api/v1/vocabularies/all | jq -r '._embedded.vocabularyList[] .name'                
```

- Check that links resolve correctly (see configuration):
```bash
curl http://localhost:8081/api/v1/records/1 | jq
```
The `_links` JSON element should contain references to other resources. These URLs should be valid and resolvable. If you are unable to open any of these references, check the configuration of the vocabulary server. Any issues regarding these URLs doesn't require a re-import of the data. Just update the configuration file and restart the vocabulary server for the changes to take effect.

## Goobi Workflow Communication
- Goobi Workflow uses the new vocabulary server since version `24.05`.
- Configure the `vocabularyServerHost` and `vocabularyServerPort` variables in `goobi_config.properties` according to your vocabulary server configuration.
- Restart Goobi Workflow for the changes to take effect.
- Navigate to `Administration` > `Vocabularies` to check if everything works. You should see a list of vocabularies if everything is okay. You will see a red error message if something doesn't work.
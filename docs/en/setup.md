# Setup
This documentation describes the process of bootstrapping the vocabulary server.

## Download and Installation
- Download [Latest Build](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/target/) of vocabulary server.
- Download [Configuration File](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/module-core/src/main/resources/application.properties) of the vocabulary server.
- Adapt configuration file properly and remove unmodified lines.
    - Database credentials and database name.
    - Base URL and port.
- **TODO** *Install the `vocabulary-server.jar` and the `application.properties` configuration file both directly into a new folder (e. g. `/opt/digiverso/vocabulary/`)*

## Start as systemd service
- **TODO** *Create a systemd service unit for the vocabulary server (The application should be able to correctly shutdown on SIGTERM)*
- **TODO** *Admin documentation here*

- Run `java -jar vocabulary-server-VERSION.jar`.
- If startup succeeds, you will see a line like this after a few seconds:
```bash
Started VocabularyServerApplication in 4.244 seconds (process running for 4.581)
```

## Goobi Workflow Setup Communication
- Goobi Workflow uses the new vocabulary server since version `24.07`.
- Configure the `vocabularyServerHost` and `vocabularyServerPort` variables in `goobi_config.properties` according to your vocabulary server configuration.
- Restart Goobi Workflow for the changes to take effect.
- Navigate to `Administration` > `Vocabularies` to check if everything works. You should see a list of vocabularies if everything is okay (not right now, but after you have created some vocabularies or migrated the existing ones). You will see a red error message if something doesn't work.

## Initial Setup
- For proper operation, the vocabulary server needs some initial data.
- This data contains language specifications (if multi-language vocabularies are used) and field type definitions. 
- You can use the following script that installs some sample languages and field types.
- Download the [Initial Data Script](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/install/default_setup.sh).
- Change the `HOST` variable at the top according to the vocabulary server configuration, leave the `/api/v1` suffix unchanged.
- Run the script

## Security
- You can setup Apache url restrictions in order to secure the vocabulary server from unauthorized access.
- **TODO** *Admins, please find out what and how to do it in detail.*

## Installation Test
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
- If a data migration has been done, check that all vocabularies have been migrated:
```bash
curl http://localhost:8081/api/v1/vocabularies/all | jq -r '._embedded.vocabularyList[] .name'                
```
- Check that links resolve correctly (see configuration):
```bash
curl http://localhost:8081/api/v1/records/1 | jq
```
The `_links` JSON element should contain references to other resources. 
These URLs should be valid and resolvable. 
If you are unable to open any of these references, check the configuration of the vocabulary server (`vocabulary-server.base-url` configuration option).
Any issues regarding these URLs doesn't require a re-import of the data. 
Just update the configuration file and restart the vocabulary server for the changes to take effect.

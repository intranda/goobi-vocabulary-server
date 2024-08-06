# Vocabulary migration
This documentation will guide you through the process of migrating existing vocabulary data to the new vocabulary server.
You can check if there are any existing vocabularies with: `mysql goobi -e "select * from vocabulary;"`.
If the output lists some vocabularies, you need to perform a migration in order to keep these vocabularies.

For all of the following instructions, the vocabulary server needs to be running.

## Set-Up instructions
First, create a virtual Python environment, activate it and install all required Python dependencies. All following instructions in this documentation always require an activated Python environment with all these dependencies present.

```bash
python -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python==8.4.0 alive_progress
```

## Perform vocabulary data migration
Download and extract the [Vocabulary Migration Tool](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/migration/*zip*/migration.zip).

**Hint** Before performing any of the following steps, please first read this documentation completely.
There is no easy "only perform these steps" solution for every case, depending on the previous data and types of vocabularies, other steps may be required.

During data migration, the Migration Tool generates new field types on-the-fly (please check out [the guide on how to create new data manually](../docs/creation.md) if you don't know what field types are or how vocabularies work in general).
Each existing vocabulary schema that contains fields with selectable values will be migrated to a new type containing these selectable values.
If you want to avoid re-creating these field types for every vocabulary or if you want to create more complex types beforehand that should be used in the migrated data, you can manually create those field types beforehand (right now) and pass this information to the migration script.
By specifying the `--lookup-file-directory` parameter, you can instruct the migration script to map existing selectable values to the given field types instead of generating new ones on-the-fly.
We will give detailed instructions on how to do this later on.

If you don't want to create any field types, you can start the data migration with the following command:
```bash
python vocabulary-migrator.py --vocabulary-server-host localhost --vocabulary-server-port 8081 --goobi-database-host localhost --goobi-database-port 3306 --goobi-database-name goobi --goobi-database-user goobi --goobi-database-password goobi --continue-on-error --fallback-language eng
```
**Hint** Change the parameters according to your configuration. The `fallback-language` parameter defines the default language to be used for a multi-lingual vocabulary field for which no default language could be derived. The `continue-on-error` option prevents the migration tool to stop on data migration errors. These errors can occur if the data could not be inserted into the new vocabulary server. Possible reasons might be:
- The vocabulary record is empty.
- The vocabulary record contains data that is incompatible with some type restrictions.
- Something unexpected.

- After the migration is done, check the file `migration_issues.log` for all unsuccessful record migrations:
    - Run `cat migration_issues.log | grep "Error validating Record" | wc -l` to get the number of unsuccessful record migrations.
    - If the record doesn't contain useful data (empty record, broken data), ignore the issue.
    - If the record contains valid information, check the reason of the failed data migration and try to fix it in the original `goobi` database (for a re-import) or add the new record manually later.
    - If the record contains valid information and you are not able to change it to work, contact the support.
    - If you have adapted any records due to migration issues, do the following for a re-import of the data:
        - Shutdown the vocabulary server.
        - Wipe the vocabulary server database.
        - Start the vocabulary server.
        - Perform the initial setup.
        - Perform the vocabulary migration as described above.
- After the migration is done, save the `migration.csv` file at a safe place.
This file contains all information about the migrated data with their old and new IDs. 
This information is mandatory to update vocabulary record references in metadata files later on.
- You should try to run the migration without the `--continue-on-error` parameter in the end. If it works, the data is migrated without any issues.

**Example**
Let's give an example of a possible migration issue (extracted from the `migration_issues.log` file):
```json
        ---------------------------------------------------------------------------------
29293
[(1441967, 29293, 1, 1, 'original value', '', 'some-value'), (1441968, 29293, 1, 2, 'corrected value', '', 'some-other-value'), (1441969, 29293, 1, 3, 'type', '', 'some-type'), (1441970, 29293, 1, 4, 'authority', '', 'Geonames'), (1441971, 29293, 1, 5, 'authorityURI', '', 'http://www.geonames.org/'), (1441972, 29293, 1, 25, 'valueURI', '', 'some-uri')]
API call was not successful, reason:
Validation error
Error validating record fields
Validation error
Error validating field value, reason(s):
Validation error
Validation error
Field values(s) "Geonames" are not one of the allowed selectable values for field "authority" [100775]: geonames,viaf
---------------------------------------------------------------------------------
```
This issue is indicating, that one of the vocabulary records fields contains the value `Geonames`.
During the migration, this field has been configured (based on existing data) with a new type, that has the following selectable values `geonames` and `viaf`.
As you can see, the present value is written with a capital `G` but only the lower-case version of `geonames` is one of the selectable values.
Therefore the validation of this vocabulary record fails and the script is unable to import this record.
In this specific case, you could update all occurrences in the old database and perform a re-import of the data afterwards.
Make sure to rename or remove the `migration_issues.log` before a re-import, because the file is appended and will contain possibly solved issues.
Any vocabulary record that is reported in this file is not imported into the vocabulary server.
If this is desired (because the vocabulary record contains bad data and should not be kept), you can just ignore this issue and go on.

See `python vocabulary-migrator.py --help` for all available options.

### The lookup-file-directory
If you plan to re-use existing field types or want to use other vocabularies for vocabulary references during a migration, create the following three files: `reference_type_lookup.csv`, `reference_value_lookup.csv` and `type_definition_lookup.csv`.
Place all three files in a new directory and pass this directory path as the `--lookup-file-directory` parameter to the Migration Tool.

To make the following configuration easier to understand, we will give an example. 
Imagine you currently have a vocabulary with a field with the following selectable values: `red`, `blue`.
Records can set any of these two values.
Multi-language support is currently achieved by creating a new field definition with another language value and providing the same amount of selectable values, this time in the other language: `rot`, `blau` (in German).
In the new vocabulary management, you can move these color specifications into a new vocabulary and replace all records that contain color values with references to this vocabulary. 
This makes it a lot easier to add colors and keep track of their translations.
For this to work, we need to create a new vocabulary `Colors` beforehand and add two records for a red and blue color.
In this example, the vocabulary has the ID `2`.
Both records contain both English and German translations.
In this example, the red color record has the ID `122` and the blue color record the ID `123`.

In addition, we want to introduce a new type that could have the values `on` and `off`.
This type should be used always if the records contain these selectable values.
In this example, the created type has the ID `21`.

Let's now create the three required files for this example:

The `type_definition_lookup.csv` looks like this:
```csv
values,type_id
on|off,21
```
This file maps all selectable values in the original field definition to the new type ID.

The `reference_type_lookup.csv` looks like this:
```csv
values,vocabulary_id
red|blue,2
rot|blau,2
```
This file maps all selectable values (in all languages, one language per line) to the ID of the vocabulary that contains the records.
Please pay attention to always define one language per line.
The reason for this is, that different languages were existing as multiple field definitions beforehand and the migration processing requires this kind of separation.
The value for the `values` columns needs to match the `selection` column in the `vocabulary_structure` table of the existing database. 

The `reference_value_lookup.csv` looks like this:
```csv
value,record_id
red,122
rot,122
blue,123
blau,123
```
This file maps all record values to the corresponding record IDs in the reference vocabulary.

## Mets file migration
This step can only be done after the vocabulary data migration has been successfully completed!

With the `migration.csv` file present, run the following command in the activated Python environment:
```bash
. vmenv/bin/activate
python metadata-migrator.py -m migration.csv -d /opt/digiverso/goobi/metadata
```

This will recursively process all mets files in all process directories.
Whenever it finds a vocabulary reference in the mets file, it will try to replace this reference with the updated information.
When anything is changed, a backup of the mets file is created beforehand.

If the mets files contain additional references to records in separate XML elements (e. g. `<goobi:metadata name="SourceID">5661</goobi:metadata>`), the `metadata-migrator.py` can also update these references with the additional parameter `--manual-id-fix SourceID`. The parameter value has to match the `name` attribute of a `metadata` element in order to replace its record id value to the new record id. This step must not be executed twice, because it would break the IDs!
   
## Data Cleanup
When the data migration is done successfully, and you are sure you will never need the old data anymore, you can manually remove all vocabulary tables from the `goobi` database of your Goobi instance:
- `vocabulary`
- `vocabulary_record`
- `vocabulary_record_data`
- `vocabulary_structure`

**Caution** The data cleanup cannot be reverted. 
If you are not sure, don't perform the cleanup steps. 
The old vocabulary data doesn't affect newer versions of Goobi workflow. 
We suggest keeping this data for some time in case anything unexpected happens.

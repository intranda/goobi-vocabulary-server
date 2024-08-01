# Vocabulary migration
This documentation will guide you through the process of migrating existing vocabulary data to the new vocabulary server.
The vocabulary server needs to be running.

## Set-Up instructions
First, create a virtual Python environment, activate it and install all required Python dependencies. All following instructions in this documentation always require an activated Python environment with all these dependencies present.

```bash
python -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python==8.4.0 alive_progress
```

## Perform vocabulary data migration
Download and extract the [Vocabulary Migration Tool](https://jenkins.intranda.com/job/intranda/job/vocabulary-server/job/develop/lastSuccessfulBuild/artifact/migration/*zip*/migration.zip). During data migration, the Migration Tool generates new types on-the-fly. Each existing vocabulary schema that contains fields with selectable values will be migrated to a new type containing these selectable values. If you wish to re-use these types or create more complex types beforehand that should be used in the migrated data, create the types beforehand and pass them to the migration script with the `--lookup-file-directory`.

You can start the data migration with the following command:
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

See `python vocabulary-migrator.py --help` for all available options.

### The lookup-file-directory
If you plan to re-use existing types or other vocabularies for vocabulary reference migrations, create the following three files: `reference_type_lookup.csv`, `reference_value_lookup.csv` and `type_definition_lookup.csv`. Place all three files in a new directory and pass this directory path as the `--lookup-file-directory` parameter to the Migration Tool.

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
This file maps all selectable values (in all languages) to the ID of the vocabulary that contains the records.

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
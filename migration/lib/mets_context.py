import os
import sys
import re

NUMBER_PATTERN = re.compile('^\\d+$')
RECORD_PATTERN = re.compile('^(\\d+).*$')

class Context:
    def __init__(self, api, dry, verbose, continue_on_error, metadata_directory, mapping_file, preferred_mets_main_value_language, manual_id_fix):
        self.api = api
        self.dry = dry
        self.verbose = verbose
        self.continue_on_error = continue_on_error
        self.metadata_directory = metadata_directory
        self.mapping_file = mapping_file
        self.preferred_mets_main_value_language = preferred_mets_main_value_language
        self.manual_id_fix = manual_id_fix
        self.vocabulary_id_name_map = {}
        self.vocabulary_id_map = {}
        self.record_id_map = {}
        self.vocabulary_id_schema_id_map = {}
        self.schema_id_main_field_id_map = {}
    
    def lookup_vocabulary_name(self, identifier):
        if not identifier in self.vocabulary_id_name_map:
            error = f'Vocabulary name not found for vocabulary with ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.vocabulary_id_name_map[identifier]

    def lookup_vocabulary_id(self, identifier):
        if not identifier in self.vocabulary_id_map:
            error = f'New vocabulary ID not found for vocabulary with old ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.vocabulary_id_map[identifier]

    def lookup_record_id(self, identifier):
        if not identifier in self.record_id_map:
            error = f'New record ID not found for record with old ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.record_id_map[identifier]

    def find_main_field_id_for_vocabulary(self, vocabulary_id):
        schema_id = self.retrieve_schema_id(vocabulary_id)
        field_id = self.retrieve_main_field_id(schema_id)
        return field_id
    
    def retrieve_schema_id(self, vocabulary_id):
        if vocabulary_id not in self.vocabulary_id_schema_id_map:
            self.vocabulary_id_schema_id_map[vocabulary_id] = self.api.lookup_vocabulary(vocabulary_id)['schemaId']
        return self.vocabulary_id_schema_id_map[vocabulary_id]
    
    def retrieve_main_field_id(self, schema_id):
        if schema_id not in self.schema_id_main_field_id_map:
            schema = self.api.lookup_schema(schema_id)
            definitions = schema['definitions']
            main_definitions = [d for d in definitions if d['mainEntry']]
            if len(main_definitions) != 1:
                logging.critical(f'Schema [{schema_id}] has no unique main entry field')
                sys.exit(1)
            self.schema_id_main_field_id_map[schema_id] = main_definitions[0]['id']
        return self.schema_id_main_field_id_map[schema_id]

    def record_contains_value(self, record, value):
        for f in record['fields']:
            for v in f['values']:
                for t in v['translations']:
                    if t['value'] == value:
                        return True
        return False

    def extract_language_values(self, field):
        lang_map = {}
        for v in field['values']:
            for t in v['translations']:
                if 'language' in t:
                    language = t['language']
                else:
                    language = ''
                value = ''
                if language in lang_map:
                    value = lang_map[language] + '|'
                value += t['value']
                lang_map[language] = value
        return lang_map

    def extract_preferred_language(self, lang_map):
        if self.preferred_mets_main_value_language in lang_map:
            return lang_map[self.preferred_mets_main_value_language]
        else:
            return list(lang_map.values())[0]
    
    def robust_find_record_id(self, parts):
          # Find first part that is a number (vocabulary ID)
        index = 0
        while index < (len(parts) - 1) and (NUMBER_PATTERN.match(parts[index]) == None or RECORD_PATTERN.match(parts[index+1]) == None):
            index += 1
        
        try:
            vocabulary_id = int(parts[index])
            m = RECORD_PATTERN.match(parts[index+1])
            record_id = int(m.group(1))
            return vocabulary_id, record_id
        except:
            return None, None

    def log_processed(self, file):
        with open('mets_migration.log', 'a') as f:
            f.write(file + '\n')

    def log_issue(self, file, reason):
        with open('mets_migration_issues.log', 'a') as f:
            f.write(f'Issue in file {file}, reason:\n\t{reason}\n')
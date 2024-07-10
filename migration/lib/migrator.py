import logging
from lib.data.schema import Schema
from lib.data.field_definition import FieldDefinition
from lib.data.vocabulary import Vocabulary
from lib.data.record import Record
from lib.data.field_instance import FieldInstance
from lib.data.field_value import FieldValue
from alive_progress import alive_bar

import time
import random

class Migrator:
    def __init__(self, ctx):
        self.ctx = ctx
    
    def migrate(self):
        self.load_existing_data_without_records()
        self.migrate_data_without_records()
        self.migrate_records()
        
    def load_existing_data_without_records(self):
        self.schemas = self.load_schemas()
        for s in self.schemas.values():
            logging.debug(f'Loaded schema:\n{s}')
        self.vocabularies = self.load_vocabularies()
        for v in self.vocabularies:
            logging.debug(f'Loaded vocabulary:\n{v}')

    def migrate_data_without_records(self):
        self.migrate_schemas()
        self.migrate_vocabularies()
    
    def migrate_schemas(self):
        logging.info(f'Migrating {len(self.schemas)} schemas')
        successful = 0
        for s in self.schemas.values():
            try:
                s.insert(self.ctx)
                self.ctx.log_processed(s)
                logging.debug(f'Schema migrated\n{s}')
                successful += 1
            except Exception as e:
                if self.ctx.continue_on_error:
                    logging.error(e)
                else:
                    raise e
        logging.info(f'{successful} of {len(self.schemas)} schemas successfully migrated')

    def migrate_vocabularies(self):
        logging.info(f'Migrating {len(self.vocabularies)} vocabularies')
        successful = 0
        for v in self.vocabularies:
            try:
                v.insert(self.ctx)
                self.ctx.log_processed(v)
                logging.debug(f'Vocabulary migrated\n{v}')
                successful += 1
            except Exception as e:
                if self.ctx.continue_on_error:
                    logging.error(e)
                else:
                    raise e
        logging.info(f'{successful} of {len(self.vocabularies)} vocabularies successfully migrated')

    def migrate_records(self):
        vocabularies_to_process = [v for v in self.vocabularies if v.is_migrated()]
        logging.info(f'Migrating records for {len(vocabularies_to_process)} vocabularies')
        for v in vocabularies_to_process:
            raw_ids = self.ctx.db.query(f'SELECT id FROM vocabulary_record WHERE vocabulary_id = {v.id}')
            ids = [i[0] for i in raw_ids]
            num = len(ids)
            with alive_bar(num) as bar:
                for chunk in list(split_into_chunks(ids, 10000)):
                    migrate_record_chunk(chunk, v, self.ctx, bar)

    def load_schemas(self):
        temp_schemas = parse_schemas(self.ctx.db.query('SELECT * FROM vocabulary_structure'))
        schemas = {}
        for k, s in temp_schemas.items():
            try:
                merge_translation_definitions(s)
                s.post_process(self.ctx.fallback_language)
                schemas[k] = s
            except Exception as e:
                if self.ctx.continue_on_error:
                    logging.error(e)
                else:
                    raise e
        logging.info(f'{len(schemas)} of {len(temp_schemas)} schemas successfully loaded')
        return schemas

    def load_vocabularies(self):
        vocabularies, errors = parse_vocabularies(self.ctx.db.query('SELECT * FROM vocabulary'), self.schemas)
        logging.info(f'{len(vocabularies)} of {len(vocabularies) + errors} vocabularies successfully loaded')
        return vocabularies

def split_into_chunks(l, n):
    for i in range(0, len(l), n):  
        yield l[i:i + n]

def parse_schemas(raw_schema):
    schema_definitions = {}
    for line in raw_schema:
        schema_id = line[1]
        if schema_id not in schema_definitions:
            schema_definitions[schema_id] = Schema(schema_id)
        schema_definitions[schema_id].add_definition(parse_definition(line))
    return schema_definitions

def parse_definition(raw_definition):
    identifier = raw_definition[0]
    result = FieldDefinition(
            id=identifier,
            name=raw_definition[2],
            language=raw_definition[3],
            itype=raw_definition[4],
            validation=raw_definition[5],
            required=raw_definition[6],
            mainEntry=raw_definition[7],
            unique=raw_definition[8],
            selection=raw_definition[9],
            titleField=raw_definition[10]
        )
    return result

def merge_translation_definitions(schema):
    unique_definitions = {}
    for definition in schema['definitions']:
        name = definition['name']
        if name not in unique_definitions:
            unique_definitions[name] = definition
        else:
            unique_definitions[name]['translationDefinitions'].append(definition['translationDefinitions'][0])
            unique_definitions[name].id.append(definition.id[0])
            if definition['mainEntry']:
                unique_definitions[name]['mainEntry'] = True
            if definition['required']:
                unique_definitions[name]['required'] = True
            if definition['unique']:
                unique_definitions[name]['unique'] = True
            if definition['titleField']:
                unique_definitions[name]['titleField'] = True
    schema['definitions'] = list(unique_definitions.values())

def parse_vocabularies(raw_vocabularies, schemas):
    vocabularies = []
    errors = 0
    for line in raw_vocabularies:
        if line[0] in schemas:
            vocabularies.append(Vocabulary(line[0], schemas[line[0]], line[1], line[2]))
        else:
            logging.error(f'Vocabulary {line[1]} can not be loaded due to unloaded schema [{line[0]}]')
            errors += 1
    return vocabularies, errors

def migrate_record_chunk(ids, vocabulary, ctx, bar):
    id_string = ','.join([str(i) for i in ids])
    raw_fields = ctx.db.query(f'SELECT * FROM vocabulary_record_data WHERE record_id in ({id_string})')
    record_field_map = {}
    for i in ids:
        record_field_map[i] = []
    for f in raw_fields:
        record_field_map[f[1]].append(f)
    for i in ids:
        migrate_record(i, record_field_map[i], vocabulary, ctx)
        bar()

def migrate_record(record_id, data, vocabulary, ctx):
    r = Record(vocabulary, record_id)
    #raw_fields = ctx.db.query(f'SELECT * FROM vocabulary_record_data WHERE record_id = {record_id}')
    if len(data) == 0:
        logging.warning(f'Skipping empty record [{record_id}')
        return
    fields = parse_fields(data, vocabulary.schema['definitions'])
    for f in fields:
        if len(f['values']) > 0:
            r.add_field(f)
    try:
        r.insert(ctx)
        ctx.log_processed(r)
        ctx.log_migration_info(r)
    except Exception as e:
        ctx.log_non_migrated_record(r, record_id, data, e)
        if ctx.continue_on_error:
            logging.warning(f'Error migrating record')
        else:
            raise e

def parse_fields(raw_fields, definitions):
    fields = {} # definitionId -> field
    for line in raw_fields:
        old_definition_id = int(line[3])
        matching_definitions = [d for d in definitions if d.matches_id(old_definition_id)]
        if len(matching_definitions) != 1:
            raise Exception('No unique definition found')
        new_definition_id = matching_definitions[0].get_new_id()

        if new_definition_id not in fields:
            fields[new_definition_id] = FieldInstance(matching_definitions[0])

        parsed_values = parse_values(language=line[5], raw_value=line[6])
        # TODO: Think about correct strategy to find merging
        for v in parsed_values:
            if len(fields[new_definition_id]['values']) == 0:
                fields[new_definition_id].add_value(v)
            else:
                # This is only one entry
                for t in v['translations']:
                    fields[new_definition_id]['values'][0].add_translation(t['language'], t['value'])
        
        # Fill translations that are required and missing with duplicates
        if 'translationDefinitions' in matching_definitions[0]:
            translation_definitions = matching_definitions[0]['translationDefinitions']
            if len(translation_definitions) > 0:
                required_languages = [td['language'] for td in translation_definitions if td['required']]
                for v in fields[new_definition_id]['values']:
                    found = [t['language'] for t in v['translations']]
                    for missing_language in [l for l in required_languages if l not in found]:
                        if len(v['translations']) > 0:
                            v.add_translation(missing_language, v['translations'][0]['value'])

    return list(fields.values())

def parse_values(language, raw_value):
    if raw_value == None:
        return []
    raw_value = raw_value.strip()
    if len(raw_value) == 0 or raw_value == 'null':
        return []
    # TODO: Test this
    raw_values = raw_value.split('|')
    results = []
    for rv in raw_values:
        result = FieldValue()
        result.add_translation(language, rv)
        results.append(result)
    return results

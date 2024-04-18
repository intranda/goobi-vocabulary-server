import logging
from lib.data.schema import Schema
from lib.data.field_definition import FieldDefinition
from lib.data.vocabulary import Vocabulary

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
        successful = 0
        for s in self.schemas.values():
            try:
                s.insert(self.ctx)
                logging.debug(f'Schema migrated\n{s}')
                successful += 1
            except Exception as e:
                if self.ctx.continue_on_error:
                    logging.error(e)
                else:
                    raise e
        logging.info(f'{successful} of {len(self.schemas)} schemas successful migrated')

    def migrate_vocabularies(self):
        successful = 0
        for v in self.vocabularies:
            try:
                v.insert(self.ctx)
                logging.debug(f'Vocabulary migrated\n{v}')
                successful += 1
            except Exception as e:
                if self.ctx.continue_on_error:
                    logging.error(e)
                else:
                    raise e
        logging.info(f'{successful} of {len(self.vocabularies)} vocabularies successful migrated')

    def migrate_records(self):
        #for v in vocabularies:
        #    raw_records = db.query(f'SELECT * FROM vocabulary_record WHERE vocabulary_id = {v.id}')
            #records = parse_records(raw_records, v, db)
        #    parse_records(raw_records, v, db)
            #for r in records:
            #    v.add_record(r)

        #for v in vocabularies:
        #    v.insert_records()
        pass
    
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

def parse_records(raw_records, vocabulary, db):
    records = []
    for line in raw_records:
        r = Record(vocabulary)
        raw_fields = query(db, f'SELECT * FROM vocabulary_record_data WHERE record_id = {line[0]}')
        fields = parse_fields(raw_fields, vocabulary.schema['definitions'])
        for f in fields:
            r.add_field(f)
        r.insert()
        records.append(r)
    return records

def parse_fields(raw_fields, definitions):
    fields = []
    for line in raw_fields:
        matching_definitions = [d for d in definitions if d.id == line[3]]
        if len(matching_definitions) != 1:
            raise Exception('No unique definition found')
        value = line[6]
        if len(value) > 0:
            f = FieldValue(matching_definitions[0], value)
            fields.append(f)
        #print(f)
    return fields
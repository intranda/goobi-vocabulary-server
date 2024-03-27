import mysql.connector
import requests
import json

SCHEMA_INSERTION_URL = 'http://localhost:8080/api/v1/schemas'
VOCABULARY_INSERTION_URL = 'http://localhost:8080/api/v1/vocabularies'
RECORD_INSERTION_URL = 'http://localhost:8080/api/v1/vocabularies/{{vocabulary_id}}/records'
HEADERS = {
    'Content-Type': 'application/json'
}

class Vocabulary(dict):
    def __init__(self, id, schema, name, description):
        self.id = id
        self.new_id = None
        self.schema = schema
        self.records = []
        self['name'] = name
        if len(description) > 0:
            self['description'] = description

    def add_record(self, record):
        self.records.append(record)
    
    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def insert(self):
        self.schema.insert()
        self['schemaId'] = self.schema.new_id
        payload = json.dumps(self)
        #print(payload)
        response = requests.request("POST", url=VOCABULARY_INSERTION_URL, headers=HEADERS, data=payload)
        #print(response.text)
        try:
            inserted = response.json()
            self.set_new_id(inserted['id'])
        except:
            print(response.text)
            raise Exception('Error')
    
    def insert_records(self):
        for r in self.records:
                r.insert()

    def __str__(self):
        return f'Vocabulary [{self.id} -> {self.new_id}] (' + self['name'] + ': ' + self['description'] + ')'

class Schema(dict):
    def __init__(self, id):
        self.id = id
        self.new_id = None
        self['definitions'] = []
    
    def add_definition(self, definition):
        self['definitions'].append(definition)
    
    def set_new_id(self, new_id):
        self.new_id = new_id

    def insert(self):
        payload = json.dumps(self)
        response = requests.request("POST", url=SCHEMA_INSERTION_URL, headers=HEADERS, data=payload)
        #print(payload)
        try:
            #print(response.text)
            inserted = response.json()
            self.set_new_id(inserted['id'])
            for d in self['definitions']:
                new_id = [nd['id'] for nd in inserted['definitions'] if nd['name'] == d['name']]
                if len(new_id) != 1:
                    raise Exception(f'Error finding new id for definition {d.__str__()}')
                d.set_new_id(new_id[0])
        except:
            print(response.text)
            raise Exception('Error')

    def __str__(self):
        return f'Schema [{self.id} -> {self.new_id}]\n\t' + '\n\t'.join([d.__str__() for d in self['definitions']])

class FieldDefinition(dict):
    def __init__(self, id, name, language, itype, validation, required, mainEntry, unique, selection, titleField):
        self.id = id
        self.new_id = None
        self['name'] = name
        self.language = language
        self.itype = itype
        self['typeId'] = 4
        self.validation = validation
        self['required'] = True if required == 1 else False
        self['mainEntry'] = True if mainEntry == 1 else False
        self['unique'] = True if unique == 1 else False
        self['titleField'] = True if titleField == 1 else False
        if self['titleField']:
            self['required'] = True
        if self['mainEntry']:
            self['unique'] = True
    
    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def __str__(self):
        return f'Definition [{self.id} -> {self.new_id}] (' + self['name'] + ')'

class Record(dict):
    def __init__(self, vocabulary):
        self['fields'] = []
        self.vocabulary = vocabulary

    def add_field(self, field):
        self['fields'].append(field)

    def insert(self):
        for f in self['fields']:
            f.insert()
        payload = json.dumps(self)
        #print(payload)
        response = requests.request("POST", url=RECORD_INSERTION_URL.replace('{{vocabulary_id}}', str(self.vocabulary.new_id)), headers=HEADERS, data=payload)
        try:
            response.json()
        except:
            print(response.text)

    def __str__(self):
        return '\n'.join([d.__str__() for d in self['fields']])

class FieldValue(dict):
    def __init__(self, definition, value):
        self.definition = definition
        self['value'] = value

    def insert(self):
        self['definitionId'] = self.definition.new_id
    
    def __str__(self):
        return f"Field [{self.definition.id} -> {self.definition.new_id}] ({self['value']})"

def setup_db_connection():
    return mysql.connector.connect(
        host="localhost",
        port='3326',
        database='goobi',
        user="goobi",
        password="goobi"
    )

def query(db, q):
    c = db.cursor()
    c.execute(q)
    return c.fetchall()

def parse_schemas(raw_schema):
    schema_definitions = {}
    for line in raw_schema:
        schema_id = line[1]
        if schema_id not in schema_definitions:
            schema_definitions[schema_id] = Schema(schema_id)
        schema_definitions[schema_id].add_definition(parse_definition(line))
    return schema_definitions

def parse_vocabularies(raw_vocabularies, schemas):
    vocabularies = []
    for line in raw_vocabularies:
        vocabularies.append(Vocabulary(line[0], schemas[line[0]], line[1], line[2]))
    return vocabularies

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

def parse_definition(raw_definition):
    id = raw_definition[0]
    result = FieldDefinition(
            id,
            raw_definition[2],
            raw_definition[3],
            raw_definition[4],
            raw_definition[5],
            raw_definition[6],
            raw_definition[7],
            raw_definition[8],
            raw_definition[9],
            raw_definition[10]
        )
    return result

def main():
    db = setup_db_connection()
    raw_schemas = query(db, 'SELECT * FROM vocabulary_structure')

    schemas = parse_schemas(raw_schemas)

    raw_vocabularies = query(db, 'SELECT * FROM vocabulary')
    vocabularies = parse_vocabularies(raw_vocabularies, schemas)

    for v in vocabularies:
        v.insert()

    for v in vocabularies:
        raw_records = query(db, f'SELECT * FROM vocabulary_record WHERE vocabulary_id = {v.id}')
        #records = parse_records(raw_records, v, db)
        parse_records(raw_records, v, db)
        #for r in records:
        #    v.add_record(r)

    #for v in vocabularies:
    #    v.insert_records()
            

if __name__ == '__main__':
    main()
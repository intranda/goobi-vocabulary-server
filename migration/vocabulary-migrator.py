import mysql.connector
import requests
import json

URL = 'http://localhost:8080/api/v1/schemas'
HEADERS = {
    'Content-Type': 'application/json'
}

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
        #print(payload)
        response = requests.request("POST", url=URL, headers=HEADERS, data=payload)
        #print(response.text)
        try:
            inserted = response.json()
            self.set_new_id(inserted['id'])
            for d in self['definitions']:
                new_id = [nd['id'] for nd in inserted['definitions'] if nd['name'] == d['name']]
                if len(new_id) != 1:
                    raise Exception(f'Error finding new id for definition {d.__str__()}')
                d.set_new_id(new_id[0])
        except:
            pass

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
    
    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def __str__(self):
        return f'Definition [{self.id} -> {self.new_id}] (' + self['name'] + ')'

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
    raw_schemas = query(db, "SELECT * FROM vocabulary_structure")

    schemas = parse_schemas(raw_schemas)

    #schemas[4].insert()    

    for k, v in schemas.items():
        print(f'{k}: {v}')

    vocabulary_id = 2
    raw_records = query(db, f'SELECT * FROM vocabulary_record WHERE vocabulary_id = {vocabulary_id}')
    for x in raw_records:
        print(x)
        raw_data = query(db, f'SELECT * FROM vocabulary_record_data WHERE record_id = {x[0]}')
        for y in raw_data:
            print(f'\t{y}')
        

if __name__ == '__main__':
    main()
import logging
import requests
import json

SCHEMA_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/schemas'
VOCABULARY_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies'
RECORD_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}/records'
RECORD_FIND_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}/records/search'
TYPE_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/types'
TYPE_FIND_URL = 'http://{{HOST}}:{{PORT}}/api/v1/types/find/{{NAME}}'
HEADERS = {
    'Content-Type': 'application/json'
}

SCHEMA_INSERTION = 1
VOCABULARY_INSERTION = 2
RECORD_INSERTION = 3
TYPE_INSERTION = 4
TYPE_SEARCH = 5
RECORD_SEARCH = 6

class API:
    def __init__(self, host, port):
        self.urls = {}
        self.urls[SCHEMA_INSERTION] = SCHEMA_INSERTION_URL
        self.urls[VOCABULARY_INSERTION] = VOCABULARY_INSERTION_URL
        self.urls[RECORD_INSERTION] = RECORD_INSERTION_URL
        self.urls[RECORD_SEARCH] = RECORD_FIND_URL
        self.urls[TYPE_INSERTION] = TYPE_INSERTION_URL
        self.urls[TYPE_SEARCH] = TYPE_FIND_URL
        self.type_id_map = {}
        self.type_counter = 1
        for u in self.urls:
            self.urls[u] = self.urls[u].replace('{{HOST}}', host).replace('{{PORT}}', port)
    
    def set_context(self, ctx):
        self.ctx = ctx

    def query(self, url, obj=None, method='POST'):
        payload = None
        if obj != None:
            payload = json.dumps(obj)
        response = requests.request(method, url=url, headers=HEADERS, data=payload)
        try:
            # Check for success
            if response.status_code // 100 != 2:
                error_msg = f'API call was not successful, reason:\n{response.text}'
                logging.warning(error_msg)
                raise Exception(error_msg)
            return response.json()
        except Exception as e:
            pretty_payload = json.dumps(obj, indent=2)
            logging.debug(f'''API request led to error
    route: {url}
    method: {method}
    headers: {HEADERS}
    payload:\n{pretty_payload}
    response: {response.text}''')
            raise e

    def insert_vocabulary(self, vocabulary):
        result = self.query(self.urls[VOCABULARY_INSERTION], vocabulary)
        return result['id']

    def insert_type(self, typ):
        name = f'_generated_{self.type_counter}'
        self.type_counter += 1
        typ['name'] = name
        result = self.query(self.urls[TYPE_INSERTION], typ)
        return result['id']
    
    def find_type(self, type_name):
        if not type_name in self.type_id_map:
            self.type_id_map[type_name] = self.query(self.urls[TYPE_SEARCH].replace('{{NAME}}', type_name), method='GET')['id']
        return self.type_id_map[type_name]
    
    def insert_schema(self, schema):
        result = self.query(self.urls[SCHEMA_INSERTION], schema)
        schema_id = result['id']
        definition_ids = {}
        for d in schema['definitions']:
            new_definition_ids = [nd['id'] for nd in result['definitions'] if nd['name'] == d['name']]
            if len(new_definition_ids) != 1:
                raise Exception(f'Error finding new id for definition {d.__str__()}')
            definition_ids[d['name']] = new_definition_ids[0]
        return schema_id, definition_ids

    def insert_record(self, record):
        url = self.urls[RECORD_INSERTION].replace('{{VOCABULARY_ID}}', str(record.vocabulary.new_id))
        if self.ctx.dry != None:
            ctx.log_api_call(url, 'POST', HEADERS, json.dumps(record))
            return
        result = self.query(url, record)
        return result['id']
    
    def find_record(self, vocabulary_id, search_term):
        url = self.urls[RECORD_SEARCH].replace('{{VOCABULARY_ID}}', str(vocabulary_id))
        url += f'?query={search_term}'
        result = self.query(url, obj=None, method='GET')
        print(result)
        results = result['_embedded']['vocabularyRecordList']
        if len(results) == 0:
            raise Exception(f'Record search for search term "{search_term}" has no results')
        elif len(results) > 1:
            raise Exception(f'Record search for search term "{search_term}" has no unique result, {len(results)} records found')
        
        return results[0]['id']


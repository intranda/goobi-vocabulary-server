import logging
import requests
import json
import sys

SCHEMA_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/schemas'
SCHEMA_LOOKUP_URL = 'http://{{HOST}}:{{PORT}}/api/v1/schemas/{{SCHEMA_ID}}'

VOCABULARY_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies'
VOCABULARY_LOOKUP_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}'

RECORD_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}/records'
RECORD_FIND_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}/records?search={{SEARCH_TERM}}'
RECORD_LOOKUP_URL = 'http://{{HOST}}:{{PORT}}/api/v1/records/{{RECORD_ID}}'

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
RECORD_LOOKUP = 7
VOCABULARY_LOOKUP = 8
SCHEMA_LOOKUP = 9

class API:
    def __init__(self, host, port, token):
        self.host = host
        self.port = port
        self.token = token
        self.urls = {}
        self.urls[SCHEMA_INSERTION] = SCHEMA_INSERTION_URL
        self.urls[VOCABULARY_INSERTION] = VOCABULARY_INSERTION_URL
        self.urls[RECORD_INSERTION] = RECORD_INSERTION_URL
        self.urls[RECORD_SEARCH] = RECORD_FIND_URL
        self.urls[RECORD_LOOKUP] = RECORD_LOOKUP_URL
        self.urls[VOCABULARY_LOOKUP] = VOCABULARY_LOOKUP_URL
        self.urls[SCHEMA_LOOKUP] = SCHEMA_LOOKUP_URL
        self.urls[TYPE_INSERTION] = TYPE_INSERTION_URL
        self.urls[TYPE_SEARCH] = TYPE_FIND_URL
        self.type_id_map = {}
        self.type_counter = 1
        for u in self.urls:
            self.urls[u] = self.urls[u].replace('{{HOST}}', host).replace('{{PORT}}', port)
        if token != None:
            HEADERS['Authorization'] = f'Bearer {token}'
    
    def set_context(self, ctx):
        self.ctx = ctx

    def query(self, url, obj=None, method='POST'):
        payload = None
        if obj != None:
            payload = json.dumps(obj)
        response = requests.request(method, url=url, headers=HEADERS, data=payload)
        try:
            # Check for success
            if response.status_code == 401 or response.status_code == 403:
                error_msg = f'API call was not successful, reason: Authentification'
                logging.critical(error_msg)
                sys.exit(1)
                raise Exception(error_msg)
            if response.status_code == 404:
                error_msg = f'API call was not successful, reason: Entity not found {url}'
                logging.warning(error_msg)
                raise Exception(error_msg)
            elif response.status_code // 100 != 2:
                error_msg = f'API call was not successful, reason:\n{extract_error_from_response(response)}'
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
    
    def find_record(self, ctx, vocabulary_id, search_term, search_field=None):
        url = self.urls[RECORD_SEARCH].replace('{{VOCABULARY_ID}}', str(vocabulary_id)).replace('{{SEARCH_TERM}}', search_term)
        result = self.query(url, obj=None, method='GET')
        if not '_embedded' in result:
            raise Exception(f'Record search in vocabulary "{vocabulary_id}" for search term "{search_term}" has no results')
        results = result['_embedded']['vocabularyRecordList']
        # Filter for exact searches
        results = [r for r in results if ctx.record_contains_value(r, search_term, search_field=search_field)]

        if len(results) == 0:
            raise Exception(f'Record search in vocabulary "{vocabulary_id}" for search term "{search_term}" has no results')
        elif len(results) > 1:
            ids = [r['id'] for r in results]
            raise Exception(f'Record search in vocabulary "{vocabulary_id}" for search term "{search_term}" has no unique result, {len(results)} records found: {ids}')
        
        return results[0]['id']

    def lookup_vocabulary(self, vocabulary_id):
        url = self.urls[VOCABULARY_LOOKUP].replace('{{VOCABULARY_ID}}', str(vocabulary_id))
        return self.query(url, obj=None, method='GET')

    def lookup_schema(self, schema_id):
        url = self.urls[SCHEMA_LOOKUP].replace('{{SCHEMA_ID}}', str(schema_id))
        return self.query(url, obj=None, method='GET')

    def lookup_record(self, record_id):
        url = self.urls[RECORD_LOOKUP].replace('{{RECORD_ID}}', str(record_id))
        return self.query(url, obj=None, method='GET')

def extract_error_from_response(response):
    try:
        return extract_error(response.json())
    except:
        return response.text

def extract_error(json):
    messages = []
    messages.append(json['message'])
    if 'causes' in json and json['causes'] != None:
        for c in json['causes']:
            messages.append(extract_error(c))
    return '\n'.join(messages)

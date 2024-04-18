import logging
import requests
import json

SCHEMA_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/schemas'
VOCABULARY_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies'
RECORD_INSERTION_URL = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{VOCABULARY_ID}}/records'
HEADERS = {
    'Content-Type': 'application/json'
}

SCHEMA_INSERTION = 1
VOCABULARY_INSERTION = 2
RECORD_INSERTION = 3

class API:
    def __init__(self, host, port):
        self.urls = {}
        self.urls[SCHEMA_INSERTION] = SCHEMA_INSERTION_URL
        self.urls[VOCABULARY_INSERTION] = VOCABULARY_INSERTION_URL
        self.urls[RECORD_INSERTION] = RECORD_INSERTION_URL
        for u in self.urls:
            self.urls[u] = self.urls[u].replace('{{HOST}}', host).replace('{{PORT}}', port)

    def query(self, url, obj, method='POST'):
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
        result = self.query(self.urls[RECORD_INSERTION].replace('{{VOCABULARY_ID}}', str(record.vocabulary.new_id)), record)
        return result['id']

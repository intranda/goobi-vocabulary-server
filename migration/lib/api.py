import logging

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

    def insert_vocabulary(self, vocabulary):
        payload = json.dumps(vocabulary)
        #print(payload)
        response = requests.request("POST", url=self.urls[VOCABULARY_INSERTION], headers=HEADERS, data=payload)
        #print(response.text)
        try:
            inserted = response.json()
            return inserted['id']
        except:
            print(response.text)
            raise Exception('Error')
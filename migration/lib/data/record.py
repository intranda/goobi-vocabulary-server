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

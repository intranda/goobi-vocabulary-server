class Record(dict):
    def __init__(self, vocabulary, id):
        self['fields'] = []
        self.id = id
        self.new_id = None
        self.vocabulary = vocabulary

    def add_field(self, field):
        self['fields'].append(field)

    def get_new_id(self):
        if self.new_id == None:
            raise Exception(f'Schema [{self.id}] not migrated yet')
        return self.new_id

    def set_new_id(self, new_id):
        self.new_id = new_id

    def insert(self, ctx):
        identifier = ctx.api.insert_record(self)
        self.set_new_id(identifier)

    def __str__(self):
        return f'Record [{self.id} -> {self.new_id}]' + '\n'.join([d.__str__() for d in self['fields']])

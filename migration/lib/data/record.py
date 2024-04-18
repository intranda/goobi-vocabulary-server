class Record(dict):
    def __init__(self, vocabulary):
        self['fields'] = []
        self.vocabulary = vocabulary

    def add_field(self, field):
        self['fields'].append(field)

    def insert(self, ctx):
        identifier = ctx.api.insert_record(self)

    def __str__(self):
        return '\n'.join([d.__str__() for d in self['fields']])

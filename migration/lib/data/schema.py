import logging

class Schema(dict):
    def __init__(self, id):
        self.id = id
        self.new_id = None
        self['definitions'] = []
    
    def add_definition(self, definition):
        self['definitions'].append(definition)

    def post_process(self, fallback_language):
        for d in self['definitions']:
            d.post_process(fallback_language)
    
    def get_new_id(self):
        if self.new_id == None:
            raise Exception(f'Schema [{self.id}] not migrated yet')
        return self.new_id

    def set_new_id(self, new_id):
        self.new_id = new_id

    def insert(self, ctx):
        identifier, definition_identifiers = ctx.api.insert_schema(self)
        self.set_new_id(identifier)
        for d in self['definitions']:
            d.set_new_id(definition_identifiers[d['name']])

    def __str__(self):
        return f'Schema [{self.id} -> {self.new_id}]\n\t' + '\n\t'.join([d.__str__() for d in self['definitions']])

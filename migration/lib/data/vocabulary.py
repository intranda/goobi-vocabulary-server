import logging

class Vocabulary(dict):
    def __init__(self, id, schema, name, description):
        self.id = id
        self.new_id = None
        self.schema = schema
        self['name'] = name
        if len(description) > 0:
            self['description'] = description
    
    def is_migrated(self):
        return self.new_id != None

    def get_new_id(self):
        if self.new_id == None:
            raise Exception(f'Vocabulary [{self.id}] not migrated yet')
        return self.new_id

    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def insert(self, ctx):
        self['schemaId'] = self.schema.get_new_id()
        identifier = ctx.api.insert_vocabulary(self)
        self.set_new_id(identifier)

    def __str__(self):
        return f'Vocabulary [{self.id} -> {self.new_id}] (' + self['name'] + (': ' + self['description'] if 'description' in self else '') + ')'

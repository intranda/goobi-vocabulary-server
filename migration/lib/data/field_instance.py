class FieldInstance(dict):
    def __init__(self, definition):
        self.definition = definition
        self['definitionId'] = definition.get_new_id()
        self['values'] = []
    
    def add_value(self, value):
        self['values'].append(value)

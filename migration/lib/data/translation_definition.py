class TranslationDefinition(dict):
    def __init__(self, language, fallback, required):
        self.new_id = None
        self['language'] = language
        self['fallback'] = fallback
        self['required'] = required
        if self['fallback']:
            self['required'] = True
    
    def get_new_id(self):
        if self.new_id == None:
            raise Exception(f'TranslationDefinition [{self.id}] not migrated yet')
        return self.new_id

    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def __str__(self):
        return f'TranslationDefinition (' + self['language'] + ', fallback=' + self['fallback'] + ', required=' + self['required'] + ')'

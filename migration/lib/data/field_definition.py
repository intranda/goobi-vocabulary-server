from lib.data.translation_definition import TranslationDefinition

class FieldDefinition(dict):
    def __init__(self, id, name, language, itype, validation, required, mainEntry, unique, selection, titleField):
        self.id = []
        self.id.append(id)
        self.new_id = None
        self['name'] = name
        self.language = language
        self.itype = itype
        self['typeId'] = 1
        self.validation = validation
        self['required'] = True if required == 1 else False
        self['mainEntry'] = True if mainEntry == 1 else False
        self['unique'] = True if unique == 1 else False
        self['titleField'] = True if titleField == 1 else False
        self.process_translation()
    
    def matches_id(self, id):
        return id in self.id

    def process_translation(self):
        if len(self.language) > 0:
            self['translationDefinitions'] = [
                TranslationDefinition(
                    language=self.language,
                    fallback=self['mainEntry'],
                    required=self['required']
                )
            ]
        
    def post_process(self, fallback_language):
        if 'translationDefinitions' in self:
            if len([t for t in self['translationDefinitions'] if t['fallback']]) == 0:
                if fallback_language == None:
                    raise Exception(f'There are optional fields with no fallback language defined, please use the "--fallback-language" parameter to set a fallback language for these cases')
                fallback_translations = [t for t in self['translationDefinitions'] if t['language'] == fallback_language]
                if len(fallback_translations) == 0:
                    raise Exception(f'Fallback language {fallback_language} is not defined for field definition:\n{self.__str__()}')
                fallback_translations[0]['fallback'] = True
                fallback_translations[0]['required'] = True
        if self['titleField']:
            self['required'] = True
        if self['mainEntry']:
            self['unique'] = True
    
    def get_new_id(self):
        if self.new_id == None:
            raise Exception(f'FieldDefinition [{self.id}] not migrated yet')
        return self.new_id

    def set_new_id(self, new_id):
        self.new_id = new_id
    
    def __str__(self):
        translations = ''
        if 'translationDefinitions' in self:
            translations = ' ' + ' '.join([f"@{td['language']}" for td in self['translationDefinitions']])
        id_str = ','.join([str(i) for i in self.id])
        return f'Definition [{id_str} -> {self.new_id}] (' + self['name'] + translations + ')'
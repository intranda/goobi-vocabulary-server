import logging
from lib.data.translation_definition import TranslationDefinition
from lib.data.field_type import FieldType

class FieldDefinition(dict):
    def __init__(self, id, name, language, itype, validation, required, mainEntry, unique, selection, titleField):
        self.id = []
        self.id.append(id)
        self.new_id = None
        self['name'] = name
        self.language = language
        self.itype = itype
        self.type_name = 'Anything'
        self.validation = validation
        if len(validation.strip()) == 0:
            self.validation = None
        self.selection = selection
        if len(selection.strip()) == 0:
            self.selection = None
        self['required'] = True if required == 1 else False
        self['mainEntry'] = True if mainEntry == 1 else False
        self['unique'] = True if unique == 1 else False
        self['titleField'] = True if titleField == 1 else False
        self['multiValued'] = True if itype == 'select' else False
        if itype == 'textarea':
            self.type_name = 'Any Text'
        self.process_translation()
    
    def matches_id(self, id):
        return id in self.id
    
    def resolve_type(self, ctx):
        if self.validation != None or self.selection != None:
            existing_type = ctx.lookup.lookup_type(self.selection)
            if existing_type != None:
                self['typeId'] = existing_type
                return

            existing_reference_vocabulary = ctx.lookup.lookup_reference_definition(self.selection)
            if existing_reference_vocabulary != None:
                self['referenceVocabularyId'] = existing_reference_vocabulary

                if 'translationDefinitions' in self:
                    del self['translationDefinitions']
                return
            
            newType = FieldType(validation=self.validation, selection=self.selection, large=self.itype == 'textarea')
            self['typeId'] = ctx.api.insert_type(newType)
        else:
            self['typeId'] = ctx.api.find_type(self.type_name)

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
            if len(self['translationDefinitions']) == 1:
                self['translationDefinitions'][0]['fallback'] = True
                self['translationDefinitions'][0]['required'] = True
            elif len([t for t in self['translationDefinitions'] if t['fallback']]) == 0:
                if fallback_language == None:
                    raise Exception(f'There are translation definitions with no fallback language defined, please use the "--fallback-language" parameter to set a fallback language for these cases')
                fallback_translations = [t for t in self['translationDefinitions'] if t['language'] == fallback_language]
                if len(fallback_translations) != 1:
                    logging.warning(f'Fallback language {fallback_language} not found or not unique for field definition:\n{self.__str__()}')
                    for ft in fallback_translations:
                        print(ft)
                    return
                fallback_translations[0]['fallback'] = True
                fallback_translations[0]['required'] = True
        if self['mainEntry']:
            self['required'] = True
    
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

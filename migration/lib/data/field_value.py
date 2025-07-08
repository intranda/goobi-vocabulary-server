from lib.data.translation_instance import TranslationInstance

class FieldValue(dict):
    def __init__(self):
        self['translations'] = []

    def post_process(self, definition):
        # Fill translations that are required and missing with duplicates
        if 'translationDefinitions' in definition:
            translation_definitions = definition['translationDefinitions']
            if len(translation_definitions) > 0:
                required_languages = [td['language'] for td in translation_definitions if td['required']]
                found = [t['language'] for t in self['translations']]
                for missing_language in [l for l in required_languages if l not in found]:
                    if len(self['translations']) > 0:
                        dv = self['translations'][0]['value']
                        logging.warn(f'Adding missing required translation for language "{missing_language}" with duplicated value "{dv}"')
                        print(v)
                        exit(1)
                        self.add_translation(missing_language, dv)

    def add_translation(self, language, value):
        self['translations'].append(TranslationInstance(language, value))
        
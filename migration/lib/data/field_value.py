class FieldValue(dict):
    def __init__(self):
        self['translations'] = {}

    def add_translation(self, language, value):
        self['translations'][language] = value
        
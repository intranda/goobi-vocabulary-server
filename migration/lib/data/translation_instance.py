class TranslationInstance(dict):
    def __init__(self, language, value):
        self['language'] = language
        self['value'] = value

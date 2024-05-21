class FieldType(dict):
    def __init__(self, validation=None, selection=None, large=False):
        self['validation'] = validation
        self['selectableValues'] = None
        if selection != None:
            self['selectableValues'] = selection.split('|')
        self['large'] = large
    
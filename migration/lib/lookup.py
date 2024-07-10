import os

class LookupDB:
    def __init__(self):
        self.type_definition_lookup = {}
        self.reference_definition_lookup = {}
        self.reference_value_lookup = {}
    
    def read_directory(self, dir):
        for filename in os.listdir(dir):
            if not filename.endswith('.csv'):
                continue
            self.parse_file(os.path.join(dir, filename))
    
    def parse_file(self, filename):
        with open(filename, 'r') as f:
            lookup_table = None
            for line in f:
                line = line.strip()
                if lookup_table == None:
                    lookup_table = self.find_lookup_table_by_header(line)
                    if lookup_table == None:
                        return
                    continue
                else:
                    parts = line.split(',')
                    lookup_table[parts[0]] = parts[1]

    def find_lookup_table_by_header(self, header):
        if header == 'values,type_id':
            return self.type_definition_lookup
        if header == 'values,vocabulary_id':
            return self.reference_definition_lookup
        if header == 'value,record_id':
            return self.reference_value_lookup
        return None

    def lookup_type(self, values):
        if values not in self.type_definition_lookup:
            return None
        return self.type_definition_lookup[values]

    def lookup_reference_definition(self, values):
        if values not in self.reference_definition_lookup:
            return None
        return self.reference_definition_lookup[values]

    def lookup_reference_value(self, value):
        if value not in self.reference_value_lookup:
            return None
        return self.reference_value_lookup[value]
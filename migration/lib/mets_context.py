import os

class Context:
    def __init__(self, api, verbose, continue_on_error, metadata_directory, mapping_file):
        self.api = api
        self.verbose = verbose
        self.continue_on_error = continue_on_error
        self.metadata_directory = metadata_directory
        self.mapping_file = mapping_file
        self.vocabulary_id_name_map = {}
        self.vocabulary_id_map = {}
        self.record_id_map = {}
    
    def lookup_vocabulary_name(self, identifier):
        if not identifier in self.vocabulary_id_name_map:
            error = f'Vocabulary name not found for vocabulary with ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.vocabulary_id_name_map[identifier]

    def lookup_vocabulary_id(self, identifier):
        if not identifier in self.vocabulary_id_map:
            error = f'New vocabulary ID not found for vocabulary with old ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.vocabulary_id_map[identifier]

    def lookup_record_id(self, identifier):
        if not identifier in self.record_id_map:
            error = f'New record ID not found for record with old ID {identifier}'
            if self.continue_on_error:
                logging.error(error)
            else:
                raise Exception(error)
        return self.record_id_map[identifier]

    def log_processed(self, file):
        with open('mets_migration.log', 'a') as f:
            f.write(file + '\n')

    def log_issue(self, file, reason):
        with open('mets_migration_issues.log', 'a') as f:
            f.write(f'Issue in file {file}, reason:\n\t{reason}\n')
import os

class Context:
    def __init__(self, db, api, dry, fallback_language, continue_on_error):
        self.db = db
        self.api = api
        self.dry = dry
        self.fallback_language = fallback_language
        self.continue_on_error = continue_on_error

    def log_processed(self, obj):
        with open('migration.log', 'a') as f:
            f.write(obj.__str__() + '\n')

    def log_migration_info(self, record):
        if not os.path.exists('migration.csv'):
            with open('migration.csv', 'w') as f:
                f.write('vocabulary_name,vocabulary_id_old,vocabulary_id_new,record_id_old,record_id_new\n')
        with open('migration.csv', 'a') as f:
            f.write(','.join([record.vocabulary['name'], str(record.vocabulary.id), str(record.vocabulary.get_new_id()), str(record.id), str(record.get_new_id())]) + '\n')
    
    def log_api_call(self, url, method, header, data):
        with open(self.dry, 'a') as f:
            f.write(f'{url} - {method} - {header} - {data}')

    def log_non_migrated_record(self, record, raw_record, raw_fields, reason):
        log = f'''
        ---------------------------------------------------------------------------------
        {raw_record}
        {raw_fields}
        {reason}
        ---------------------------------------------------------------------------------
        
        '''
        with open('migration_issues.log', 'a') as f:
            f.write(log)

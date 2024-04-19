class Context:
    def __init__(self, db, api, dry, fallback_language, continue_on_error):
        self.db = db
        self.api = api
        self.dry = dry
        self.fallback_language = fallback_language
        self.continue_on_error = continue_on_error
    
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

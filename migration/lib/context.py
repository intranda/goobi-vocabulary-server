class Context:
    def __init__(self, db, api, fallback_language, continue_on_error):
        self.db = db
        self.api = api
        self.fallback_language = fallback_language
        self.continue_on_error = continue_on_error
    
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

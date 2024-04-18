class Context:
    def __init__(self, db, api, fallback_language, continue_on_error):
        self.db = db
        self.api = api
        self.fallback_language = fallback_language
        self.continue_on_error = continue_on_error
    
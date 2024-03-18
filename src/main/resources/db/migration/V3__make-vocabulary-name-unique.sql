ALTER TABLE vocabulary
    ADD CONSTRAINT uc_vocabulary_name UNIQUE (name);
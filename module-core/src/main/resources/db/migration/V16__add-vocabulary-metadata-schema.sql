ALTER TABLE vocabulary
    ADD metadata_schema_id BIGINT NULL;

ALTER TABLE vocabulary
    ADD CONSTRAINT FK_VOCABULARY_ON_METADATA_SCHEMA FOREIGN KEY (metadata_schema_id) REFERENCES vocabulary_schema (id);
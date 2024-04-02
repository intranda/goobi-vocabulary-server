ALTER TABLE vocabulary_schema
    ADD hierarchical_records BIT(1) NULL;

ALTER TABLE vocabulary_record
    ADD parent_record_id BIGINT NULL;

ALTER TABLE vocabulary_record
    ADD CONSTRAINT FK_VOCABULARYRECORD_ON_PARENT_RECORD FOREIGN KEY (parent_record_id) REFERENCES vocabulary_record (id);

ALTER TABLE field_value
    MODIFY field_instance_ BIGINT NOT NULL;

ALTER TABLE field_translation
    MODIFY field_value_id BIGINT NOT NULL;
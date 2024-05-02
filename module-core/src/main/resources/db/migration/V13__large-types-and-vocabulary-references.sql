ALTER TABLE field_type
    ADD large BIT(1) NULL;

ALTER TABLE field_definition
    ADD reference_vocabulary_id BIGINT NULL;

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELD_DEFINITION_ON_REFERENCE_VOCABULARY FOREIGN KEY (reference_vocabulary_id) REFERENCES vocabulary (id);
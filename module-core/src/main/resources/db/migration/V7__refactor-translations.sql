ALTER TABLE field_value_translation_mapping
DROP
FOREIGN KEY fk_field_value_translation_mapping_on_field_value;

ALTER TABLE field_value_translation_mapping
DROP
FOREIGN KEY fk_field_value_translation_mapping_on_language;

CREATE SEQUENCE field_translation_seq INCREMENT BY 50 START WITH 1;

CREATE TABLE field_translation
(
    id             BIGINT NOT NULL,
    field_value_id BIGINT NULL,
    language_id    BIGINT NOT NULL,
    content        LONGTEXT NULL,
    CONSTRAINT pk_fieldtranslation PRIMARY KEY (id)
);

ALTER TABLE field_translation
    ADD CONSTRAINT FK_FIELDTRANSLATION_ON_FIELD_VALUE FOREIGN KEY (field_value_id) REFERENCES field_value (id);

ALTER TABLE field_translation
    ADD CONSTRAINT FK_FIELDTRANSLATION_ON_LANGUAGE FOREIGN KEY (language_id) REFERENCES language (id);

DROP TABLE field_value_translation_mapping;
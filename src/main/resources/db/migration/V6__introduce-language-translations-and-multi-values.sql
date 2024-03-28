CREATE SEQUENCE field_value_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE language_seq INCREMENT BY 50 START WITH 1;

CREATE TABLE field_value
(
    id              BIGINT NOT NULL,
    field_instance_ BIGINT NULL,
    CONSTRAINT pk_fieldvalue PRIMARY KEY (id)
);

CREATE TABLE field_value_translation_mapping
(
    field_value_id BIGINT NOT NULL,
    translations   VARCHAR(255) NULL,
    language_id    BIGINT NOT NULL,
    CONSTRAINT pk_field_value_translation_mapping PRIMARY KEY (field_value_id, language_id)
);

CREATE TABLE language
(
    id           BIGINT       NOT NULL,
    abbreviation VARCHAR(255) NOT NULL,
    full_name    VARCHAR(255) NULL,
    CONSTRAINT pk_language PRIMARY KEY (id)
);

ALTER TABLE language
    ADD CONSTRAINT uc_language_abbreviation UNIQUE (abbreviation);

ALTER TABLE field_value
    ADD CONSTRAINT FK_FIELDVALUE_ON_FIELD_INSTANCE_ FOREIGN KEY (field_instance_) REFERENCES field_instance (id);

ALTER TABLE field_value_translation_mapping
    ADD CONSTRAINT fk_field_value_translation_mapping_on_field_value FOREIGN KEY (field_value_id) REFERENCES field_value (id);

ALTER TABLE field_value_translation_mapping
    ADD CONSTRAINT fk_field_value_translation_mapping_on_language FOREIGN KEY (language_id) REFERENCES language (id);

ALTER TABLE field_instance
DROP
COLUMN content;

ALTER TABLE field_instance
DROP
COLUMN language;
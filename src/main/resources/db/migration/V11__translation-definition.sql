CREATE SEQUENCE translation_definition_seq INCREMENT BY 50 START WITH 1;

CREATE TABLE translation_definition
(
    id                  BIGINT NOT NULL,
    language_id         BIGINT NOT NULL,
    fallback            BIT(1) NULL,
    field_definition_id BIGINT NULL,
    required            BIT(1) NOT NULL,
    CONSTRAINT pk_translation_definition PRIMARY KEY (id)
);

ALTER TABLE language
    ADD is_default BIT(1) NULL;

ALTER TABLE translation_definition
    ADD CONSTRAINT uc_237dd75d39305be1cae4ab970 UNIQUE (field_definition_id, fallback);

ALTER TABLE language
    ADD CONSTRAINT uc_language_is_default UNIQUE (is_default);

ALTER TABLE translation_definition
    ADD CONSTRAINT FK_TRANSLATION_DEFINITION_ON_FIELD_DEFINITION FOREIGN KEY (field_definition_id) REFERENCES field_definition (id);

ALTER TABLE translation_definition
    ADD CONSTRAINT FK_TRANSLATION_DEFINITION_ON_LANGUAGE FOREIGN KEY (language_id) REFERENCES language (id);
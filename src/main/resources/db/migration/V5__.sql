CREATE TABLE field_definition
(
    id            BIGINT       NOT NULL,
    schema_id     INT          NOT NULL,
    language      VARCHAR(3)   NULL,
    name          VARCHAR(255) NOT NULL,
    type_id       BIGINT       NOT NULL,
    validation_id INT          NULL,
    required      BIT(1)       NOT NULL,
    `unique`      BIT(1)       NOT NULL,
    main_entry    BIT(1)       NOT NULL,
    title_field   BIT(1)       NOT NULL,
    CONSTRAINT pk_fielddefinition PRIMARY KEY (id)
);

ALTER TABLE field_definition
    ADD CONSTRAINT uc_fielddefinition_name UNIQUE (name);

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELDDEFINITION_ON_SCHEMA FOREIGN KEY (schema_id) REFERENCES `schema` (id);

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELDDEFINITION_ON_TYPE FOREIGN KEY (type_id) REFERENCES field_type (id);

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELDDEFINITION_ON_VALIDATION FOREIGN KEY (validation_id) REFERENCES field_validation (id);

ALTER TABLE field_instance
    ADD CONSTRAINT FK_FIELDINSTANCE_ON_FIELD_DEFINITION FOREIGN KEY (field_definition_id) REFERENCES field_definition (id);

ALTER TABLE field_instance
    MODIFY value LONGTEXT;
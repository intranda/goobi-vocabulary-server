CREATE SEQUENCE field_definition_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE field_instance_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE field_type_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE field_validation_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE selectable_value_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE vocabulary_record_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE vocabulary_schema_seq INCREMENT BY 50 START WITH 1;

CREATE SEQUENCE vocabulary_seq INCREMENT BY 50 START WITH 1;

CREATE TABLE field_definition
(
    id          BIGINT       NOT NULL,
    schema_id   BIGINT       NOT NULL,
    language    VARCHAR(3)   NULL,
    label       VARCHAR(255) NOT NULL,
    type_id     BIGINT       NOT NULL,
    required    BIT(1)       NOT NULL,
    distinctive BIT(1)       NOT NULL,
    main_entry  BIT(1)       NULL,
    title_field BIT(1)       NOT NULL,
    CONSTRAINT pk_fielddefinition PRIMARY KEY (id)
);

CREATE TABLE field_instance
(
    id                  BIGINT       NOT NULL,
    field_definition_id BIGINT       NOT NULL,
    record_id           BIGINT       NOT NULL,
    language            VARCHAR(3)   NULL,
    content             VARCHAR(255) NULL,
    CONSTRAINT pk_fieldinstance PRIMARY KEY (id)
);

CREATE TABLE field_type
(
    id            BIGINT       NOT NULL,
    type_name     VARCHAR(255) NOT NULL,
    validation_id BIGINT       NULL,
    CONSTRAINT pk_fieldtype PRIMARY KEY (id)
);

CREATE TABLE field_type_selectable_values
(
    field_type_id        BIGINT NOT NULL,
    selectable_values_id BIGINT NOT NULL,
    CONSTRAINT pk_fieldtype_selectablevalues PRIMARY KEY (field_type_id, selectable_values_id)
);

CREATE TABLE field_validation
(
    id                 BIGINT        NOT NULL,
    regular_expression VARCHAR(1024) NOT NULL,
    CONSTRAINT pk_fieldvalidation PRIMARY KEY (id)
);

CREATE TABLE selectable_value
(
    id              BIGINT       NOT NULL,
    selection_value VARCHAR(255) NOT NULL,
    CONSTRAINT pk_selectablevalue PRIMARY KEY (id)
);

CREATE TABLE vocabulary
(
    id            BIGINT        NOT NULL,
    schema_id     BIGINT        NOT NULL,
    title         VARCHAR(255)  NOT NULL,
    `description` VARCHAR(4096) NULL,
    CONSTRAINT pk_vocabulary PRIMARY KEY (id)
);

CREATE TABLE vocabulary_record
(
    id            BIGINT NOT NULL,
    vocabulary_id BIGINT NOT NULL,
    CONSTRAINT pk_vocabularyrecord PRIMARY KEY (id)
);

CREATE TABLE vocabulary_schema
(
    id BIGINT NOT NULL,
    CONSTRAINT pk_vocabularyschema PRIMARY KEY (id)
);

ALTER TABLE field_definition
    ADD CONSTRAINT uc_46e8e276223be04d41c2677f0 UNIQUE (schema_id, label);

ALTER TABLE field_definition
    ADD CONSTRAINT uc_5da3ceb45920df55de6c71310 UNIQUE (schema_id, main_entry);

ALTER TABLE field_type
    ADD CONSTRAINT uc_fieldtype_type_name UNIQUE (type_name);

ALTER TABLE vocabulary
    ADD CONSTRAINT uc_vocabulary_title UNIQUE (title);

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELDDEFINITION_ON_SCHEMA FOREIGN KEY (schema_id) REFERENCES vocabulary_schema (id);

ALTER TABLE field_definition
    ADD CONSTRAINT FK_FIELDDEFINITION_ON_TYPE FOREIGN KEY (type_id) REFERENCES field_type (id);

ALTER TABLE field_instance
    ADD CONSTRAINT FK_FIELDINSTANCE_ON_FIELD_DEFINITION FOREIGN KEY (field_definition_id) REFERENCES field_definition (id);

ALTER TABLE field_instance
    ADD CONSTRAINT FK_FIELDINSTANCE_ON_RECORD FOREIGN KEY (record_id) REFERENCES vocabulary_record (id);

ALTER TABLE field_type
    ADD CONSTRAINT FK_FIELDTYPE_ON_VALIDATION FOREIGN KEY (validation_id) REFERENCES field_validation (id);

ALTER TABLE vocabulary_record
    ADD CONSTRAINT FK_VOCABULARYRECORD_ON_VOCABULARY FOREIGN KEY (vocabulary_id) REFERENCES vocabulary (id);

ALTER TABLE vocabulary
    ADD CONSTRAINT FK_VOCABULARY_ON_SCHEMA FOREIGN KEY (schema_id) REFERENCES vocabulary_schema (id);

ALTER TABLE field_type_selectable_values
    ADD CONSTRAINT fk_fietypselval_on_field_type FOREIGN KEY (field_type_id) REFERENCES field_type (id);

ALTER TABLE field_type_selectable_values
    ADD CONSTRAINT fk_fietypselval_on_selectable_value FOREIGN KEY (selectable_values_id) REFERENCES selectable_value (id);
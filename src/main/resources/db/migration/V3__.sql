ALTER TABLE vocabulary
    ADD `description` VARCHAR(4096) NULL;

ALTER TABLE vocabulary
    ADD name VARCHAR(255) NULL;

ALTER TABLE field_type
    ADD name VARCHAR(255) NULL;

ALTER TABLE field_type
    MODIFY name VARCHAR (255) NOT NULL;

ALTER TABLE vocabulary
    MODIFY name VARCHAR (255) NOT NULL;

ALTER TABLE field_type
    ADD CONSTRAINT uc_fieldtype_name UNIQUE (name);
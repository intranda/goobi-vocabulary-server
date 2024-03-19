ALTER TABLE field_definition
    DROP FOREIGN KEY FK_FIELDDEFINITION_ON_VALIDATION;

ALTER TABLE field_type
    ADD validation_id INT NULL;

ALTER TABLE field_type
    ADD CONSTRAINT FK_FIELDTYPE_ON_VALIDATION FOREIGN KEY (validation_id) REFERENCES field_validation (id);

ALTER TABLE field_definition
    DROP COLUMN validation_id;
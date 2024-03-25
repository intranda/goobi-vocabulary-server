ALTER TABLE field_type
DROP
FOREIGN KEY FK_FIELDTYPE_ON_VALIDATION;

ALTER TABLE field_type
    ADD validation VARCHAR(255) NULL;

DROP TABLE field_validation;

ALTER TABLE field_type
DROP
COLUMN validation_id;

DROP SEQUENCE field_validation_seq;
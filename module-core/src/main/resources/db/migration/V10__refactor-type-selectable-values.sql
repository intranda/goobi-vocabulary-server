ALTER TABLE field_value
DROP
FOREIGN KEY FK_FIELDVALUE_ON_FIELD_INSTANCE_;

ALTER TABLE field_type_selectable_values
DROP
FOREIGN KEY fk_fietypselval_on_field_type;

ALTER TABLE field_type_selectable_values
DROP
FOREIGN KEY fk_fietypselval_on_selectable_value;

ALTER TABLE field_value
    ADD field_instance_id BIGINT NULL;

ALTER TABLE field_value
    MODIFY field_instance_id BIGINT NOT NULL;

ALTER TABLE selectable_value
    ADD field_type_id BIGINT NULL;

ALTER TABLE selectable_value
    MODIFY field_type_id BIGINT NOT NULL;

ALTER TABLE field_value
    ADD CONSTRAINT FK_FIELDVALUE_ON_FIELD_INSTANCE FOREIGN KEY (field_instance_id) REFERENCES field_instance (id);

ALTER TABLE selectable_value
    ADD CONSTRAINT FK_SELECTABLEVALUE_ON_FIELD_TYPE FOREIGN KEY (field_type_id) REFERENCES field_type (id);

DROP TABLE field_type_selectable_values;

ALTER TABLE field_value
DROP
COLUMN field_instance_;
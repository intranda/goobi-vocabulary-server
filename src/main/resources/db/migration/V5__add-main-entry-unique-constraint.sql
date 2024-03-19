ALTER TABLE field_definition
    ADD CONSTRAINT uc_5da3ceb45920df55de6c71310 UNIQUE (schema_id, main_entry);

ALTER TABLE field_definition
    MODIFY main_entry BIT(1) NULL;
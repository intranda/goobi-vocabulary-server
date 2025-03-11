DROP TABLE field_definition_seq;

DROP TABLE field_instance_seq;

DROP TABLE field_translation_seq;

DROP TABLE field_type_seq;

DROP TABLE field_value_seq;

DROP TABLE language_seq;

DROP TABLE selectable_value_seq;

DROP TABLE translation_definition_seq;

DROP TABLE vocabulary_record_seq;

DROP TABLE vocabulary_schema_seq;

DROP TABLE vocabulary_seq;

ALTER TABLE field_definition
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE field_instance
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE field_translation
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE field_type
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE field_value
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE language
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE selectable_value
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE translation_definition
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE vocabulary
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE vocabulary_record
    MODIFY id BIGINT AUTO_INCREMENT;

ALTER TABLE vocabulary_schema
    MODIFY id BIGINT AUTO_INCREMENT;
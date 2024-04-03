package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.FieldValueDTO;
import io.goobi.vocabularyserver.exchange.LanguageDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.FieldValue;
import io.goobi.vocabularyserver.model.Language;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;

public interface DTOMapper {
    default FieldDefinition toEntity(FieldDefinitionDTO dto) {
        return toEntity(dto, true);
    }
    FieldDefinition toEntity(FieldDefinitionDTO dto, boolean fullInitialization);

    FieldDefinitionDTO toDTO(FieldDefinition entity);

    default FieldInstance toEntity(FieldInstanceDTO dto) {
        return toEntity(dto, true);
    }
    FieldInstance toEntity(FieldInstanceDTO dto, boolean fullInitialization);

    FieldInstanceDTO toDTO(FieldInstance entity);

    default FieldValue toEntity(FieldValueDTO dto) {
        return toEntity(dto, true);
    }
    FieldValue toEntity(FieldValueDTO dto, boolean fullInitialization);
    FieldValueDTO toDTO(FieldValue entity);

    FieldType toEntity(FieldTypeDTO dto);

    FieldTypeDTO toDTO(FieldType entity);

    Vocabulary toEntity(VocabularyDTO dto);

    VocabularyDTO toDTO(Vocabulary entity);

    VocabularySchema toEntity(VocabularySchemaDTO dto);

    VocabularySchemaDTO toDTO(VocabularySchema entity);

    VocabularyRecord toEntity(VocabularyRecordDTO dto);

    VocabularyRecordDTO toDTO(VocabularyRecord entity);

    Language toEntity(LanguageDTO dto);
    LanguageDTO toDTO(Language entity);
}

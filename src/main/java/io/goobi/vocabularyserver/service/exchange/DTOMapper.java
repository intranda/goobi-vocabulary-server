package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinitionDTO;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.FieldTypeDTO;
import io.goobi.vocabularyserver.exchange.VocabularyDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.exchange.VocabularySchemaDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
import io.goobi.vocabularyserver.model.VocabularySchema;

public interface DTOMapper {
    default FieldDefinition toEntity(FieldDefinitionDTO dto) {
        return toEntity(dto, true);
    }
    FieldDefinition toEntity(FieldDefinitionDTO dto, boolean fullInitialization);

    FieldDefinitionDTO toDTO(FieldDefinition entity);

    FieldInstance toEntity(FieldInstanceDTO dto);

    FieldInstanceDTO toDTO(FieldInstance entity);

    FieldType toEntity(FieldTypeDTO dto);

    FieldTypeDTO toDTO(FieldType entity);

    Vocabulary toEntity(VocabularyDTO dto);

    VocabularyDTO toDTO(Vocabulary entity);

    VocabularySchema toEntity(VocabularySchemaDTO dto);

    VocabularySchemaDTO toDTO(VocabularySchema entity);

    VocabularyRecord toEntity(VocabularyRecordDTO dto);

    VocabularyRecordDTO toDTO(VocabularyRecord entity);
}

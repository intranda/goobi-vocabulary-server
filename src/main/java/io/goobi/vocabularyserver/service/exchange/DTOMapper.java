package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exchange.FieldDefinition;
import io.goobi.vocabularyserver.exchange.FieldInstance;
import io.goobi.vocabularyserver.exchange.FieldType;
import io.goobi.vocabularyserver.exchange.FieldValue;
import io.goobi.vocabularyserver.exchange.Language;
import io.goobi.vocabularyserver.exchange.TranslationDefinition;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.model.FieldDefinitionEntity;
import io.goobi.vocabularyserver.model.FieldInstanceEntity;
import io.goobi.vocabularyserver.model.FieldTypeEntity;
import io.goobi.vocabularyserver.model.FieldValueEntity;
import io.goobi.vocabularyserver.model.LanguageEntity;
import io.goobi.vocabularyserver.model.TranslationDefinitionEntity;
import io.goobi.vocabularyserver.model.VocabularyEntity;
import io.goobi.vocabularyserver.model.VocabularyRecordEntity;
import io.goobi.vocabularyserver.model.VocabularySchemaEntity;

public interface DTOMapper {
    default FieldDefinitionEntity toEntity(FieldDefinition dto) {
        return toEntity(dto, true);
    }
    FieldDefinitionEntity toEntity(FieldDefinition dto, boolean fullInitialization);

    FieldDefinition toDTO(FieldDefinitionEntity entity);

    TranslationDefinitionEntity toEntity(TranslationDefinition dto);

    TranslationDefinition toDTO(TranslationDefinitionEntity entity);

    default FieldInstanceEntity toEntity(FieldInstance dto) {
        return toEntity(dto, true);
    }
    FieldInstanceEntity toEntity(FieldInstance dto, boolean fullInitialization);

    FieldInstance toDTO(FieldInstanceEntity entity);

    default FieldValueEntity toEntity(FieldValue dto) {
        return toEntity(dto, true);
    }
    FieldValueEntity toEntity(FieldValue dto, boolean fullInitialization);
    FieldValue toDTO(FieldValueEntity entity);

    FieldTypeEntity toEntity(FieldType dto);

    FieldType toDTO(FieldTypeEntity entity);

    VocabularyEntity toEntity(Vocabulary dto);

    Vocabulary toDTO(VocabularyEntity entity);

    VocabularySchemaEntity toEntity(VocabularySchema dto);

    VocabularySchema toDTO(VocabularySchemaEntity entity);

    VocabularyRecordEntity toEntity(VocabularyRecord dto);

    VocabularyRecord toDTO(VocabularyRecordEntity entity);

    LanguageEntity toEntity(Language dto);
    Language toDTO(LanguageEntity entity);
}

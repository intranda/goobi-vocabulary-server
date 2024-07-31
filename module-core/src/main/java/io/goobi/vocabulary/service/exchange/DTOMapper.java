package io.goobi.vocabulary.service.exchange;

import io.goobi.vocabulary.exchange.FieldDefinition;
import io.goobi.vocabulary.exchange.FieldInstance;
import io.goobi.vocabulary.exchange.FieldType;
import io.goobi.vocabulary.exchange.FieldValue;
import io.goobi.vocabulary.exchange.Language;
import io.goobi.vocabulary.exchange.TranslationDefinition;
import io.goobi.vocabulary.exchange.Vocabulary;
import io.goobi.vocabulary.exchange.VocabularyRecord;
import io.goobi.vocabulary.exchange.VocabularySchema;
import io.goobi.vocabulary.model.jpa.FieldDefinitionEntity;
import io.goobi.vocabulary.model.jpa.FieldInstanceEntity;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.FieldValueEntity;
import io.goobi.vocabulary.model.jpa.LanguageEntity;
import io.goobi.vocabulary.model.jpa.TranslationDefinitionEntity;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import io.goobi.vocabulary.model.jpa.VocabularyRecordEntity;
import io.goobi.vocabulary.model.jpa.VocabularySchemaEntity;

public interface DTOMapper {
    default FieldDefinitionEntity toEntity(FieldDefinition dto) {
        return toEntity(dto, true);
    }

    FieldDefinitionEntity toEntity(FieldDefinition dto, boolean fullInitialization);

    FieldDefinition toDTO(FieldDefinitionEntity entity);

    default TranslationDefinitionEntity toEntity(TranslationDefinition dto) {
        return toEntity(dto, true);
    }

    TranslationDefinitionEntity toEntity(TranslationDefinition dto, boolean fullInitialization);

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

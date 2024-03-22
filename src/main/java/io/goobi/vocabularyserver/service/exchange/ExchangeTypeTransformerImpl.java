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
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Service
public class ExchangeTypeTransformerImpl implements ExchangeTypeTransformer {
    @Override
    public FieldDefinitionDTO transform(FieldDefinition jpaFieldDefinition) {
        FieldDefinitionDTO exchangeFieldDefinitionDTO = new FieldDefinitionDTO();
        exchangeFieldDefinitionDTO.setId(jpaFieldDefinition.getId());
        exchangeFieldDefinitionDTO.setLanguage(jpaFieldDefinition.getLanguage());
        exchangeFieldDefinitionDTO.setName(jpaFieldDefinition.getName());
        exchangeFieldDefinitionDTO.setTypeId(jpaFieldDefinition.getType().getId());
        exchangeFieldDefinitionDTO.setRequired(jpaFieldDefinition.getRequired());
        exchangeFieldDefinitionDTO.setUnique(jpaFieldDefinition.getUnique());
        exchangeFieldDefinitionDTO.setMainEntry(jpaFieldDefinition.getMainEntry() != null);
        exchangeFieldDefinitionDTO.setTitleField(jpaFieldDefinition.getTitleField());
        return exchangeFieldDefinitionDTO;
    }

    @Override
    public FieldTypeDTO transform(FieldType jpaFieldType) {
        FieldTypeDTO exchangeFieldTypeDTO = new FieldTypeDTO();
        exchangeFieldTypeDTO.setId(jpaFieldType.getId());
        exchangeFieldTypeDTO.setName(jpaFieldType.getName());
        return exchangeFieldTypeDTO;
    }

    @Override
    public VocabularyDTO transform(Vocabulary jpaVocabulary) {
        VocabularyDTO exchangeVocabularyDTO = new VocabularyDTO();
        exchangeVocabularyDTO.setId(jpaVocabulary.getId());
        exchangeVocabularyDTO.setName(jpaVocabulary.getName());
        exchangeVocabularyDTO.setDescription(jpaVocabulary.getDescription());
        exchangeVocabularyDTO.setSchemaId(jpaVocabulary.getSchema().getId());
        return exchangeVocabularyDTO;
    }

    @Override
    public VocabularySchemaDTO transform(VocabularySchema jpaVocabularySchema) {
        VocabularySchemaDTO exchangeVocabularySchemaDTO = new VocabularySchemaDTO();
        exchangeVocabularySchemaDTO.setId(jpaVocabularySchema.getId());
        exchangeVocabularySchemaDTO.setDefinitions(jpaVocabularySchema.getDefinitions()
                .stream()
                .map(this::transform)
                .collect(Collectors.toList()));
        return exchangeVocabularySchemaDTO;
    }

    @Override
    public VocabularyRecordDTO transform(VocabularyRecord jpaVocabularyRecord) {
        VocabularyRecordDTO exchangeVocabularyRecordDTO = new VocabularyRecordDTO();
        exchangeVocabularyRecordDTO.setId(jpaVocabularyRecord.getId());
        exchangeVocabularyRecordDTO.setVocabularyId(jpaVocabularyRecord.getVocabulary().getId());
        exchangeVocabularyRecordDTO.setFields(jpaVocabularyRecord.getFields()
                .stream()
                .map(this::transform)
                .collect(Collectors.toSet()));
        return exchangeVocabularyRecordDTO;
    }

    @Override
    public FieldInstanceDTO transform(FieldInstance jpaFieldInstance) {
        FieldInstanceDTO exchangeFieldInstanceDTO = new FieldInstanceDTO();
        exchangeFieldInstanceDTO.setId(jpaFieldInstance.getId());
        exchangeFieldInstanceDTO.setDefinitionId(jpaFieldInstance.getDefinition().getId());
        exchangeFieldInstanceDTO.setLanguage(jpaFieldInstance.getLanguage());
        exchangeFieldInstanceDTO.setValue(jpaFieldInstance.getValue());
        return exchangeFieldInstanceDTO;
    }
}

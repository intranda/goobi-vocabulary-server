package io.goobi.vocabularyserver.service.exchange;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldDefinition;
import io.goobi.vocabularyserver.exchange.FieldInstance;
import io.goobi.vocabularyserver.exchange.FieldType;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

@Service
public class ExchangeTypeTransformerImpl implements ExchangeTypeTransformer {

    private final VocabularySchemaRepository vocabularySchemaRepository;

    public ExchangeTypeTransformerImpl(VocabularySchemaRepository vocabularySchemaRepository) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
    }

    @Override
    public FieldDefinition transform(io.goobi.vocabularyserver.model.FieldDefinition jpaFieldDefinition) {
        FieldDefinition exchangeFieldDefinition = new FieldDefinition();
        exchangeFieldDefinition.setId(jpaFieldDefinition.getId());
        exchangeFieldDefinition.setLanguage(jpaFieldDefinition.getLanguage());
        exchangeFieldDefinition.setName(jpaFieldDefinition.getName());
        exchangeFieldDefinition.setType(transform(jpaFieldDefinition.getType()));
        exchangeFieldDefinition.setRequired(jpaFieldDefinition.getRequired());
        exchangeFieldDefinition.setUnique(jpaFieldDefinition.getUnique());
        exchangeFieldDefinition.setMainEntry(jpaFieldDefinition.getMainEntry() != null);
        exchangeFieldDefinition.setTitleField(jpaFieldDefinition.getTitleField());
        return exchangeFieldDefinition;
    }

    @Override
    public FieldType transform(io.goobi.vocabularyserver.model.FieldType jpaFieldType) {
        FieldType exchangeFieldType = new FieldType();
        exchangeFieldType.setId(jpaFieldType.getId());
        exchangeFieldType.setName(jpaFieldType.getName());
        return exchangeFieldType;
    }

    @Override
    public Vocabulary transform(io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary) {
        Vocabulary exchangeVocabulary = new Vocabulary();
        exchangeVocabulary.setId(jpaVocabulary.getId());
        exchangeVocabulary.setName(jpaVocabulary.getName());
        exchangeVocabulary.setDescription(jpaVocabulary.getDescription());
        exchangeVocabulary.setSchemaId(jpaVocabulary.getSchema().getId());
        return exchangeVocabulary;
    }

    @Override
    public io.goobi.vocabularyserver.model.Vocabulary transform(Vocabulary newVocabulary) {
        io.goobi.vocabularyserver.model.VocabularySchema jpaVocabularySchema = vocabularySchemaRepository
                .findById(newVocabulary.getSchemaId())
                .orElseThrow(() -> new EntityNotFoundException("VocabularySchema", newVocabulary.getSchemaId()));
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = new io.goobi.vocabularyserver.model.Vocabulary(jpaVocabularySchema, newVocabulary.getName());
        jpaVocabulary.setDescription(newVocabulary.getDescription());
        return jpaVocabulary;
    }

    @Override
    public VocabularySchema transform(io.goobi.vocabularyserver.model.VocabularySchema jpaVocabularySchema) {
        VocabularySchema exchangeVocabularySchema = new VocabularySchema();
        exchangeVocabularySchema.setId(jpaVocabularySchema.getId());
        exchangeVocabularySchema.setDefinitions(jpaVocabularySchema.getDefinitions()
                .stream()
                .map(this::transform)
                .collect(Collectors.toList()));
        return exchangeVocabularySchema;
    }

    @Override
    public VocabularyRecord transform(io.goobi.vocabularyserver.model.VocabularyRecord jpaVocabularyRecord) {
        VocabularyRecord exchangeVocabularyRecord = new VocabularyRecord();
        exchangeVocabularyRecord.setId(jpaVocabularyRecord.getId());
        exchangeVocabularyRecord.setFields(jpaVocabularyRecord.getFields()
                .stream()
                .map(this::transform)
                .collect(Collectors.toSet()));
        return exchangeVocabularyRecord;
    }

    @Override
    public FieldInstance transform(io.goobi.vocabularyserver.model.FieldInstance jpaFieldInstance) {
        FieldInstance exchangeFieldInstance = new FieldInstance();
        exchangeFieldInstance.setId(jpaFieldInstance.getId());
        exchangeFieldInstance.setLanguage(jpaFieldInstance.getLanguage());
        exchangeFieldInstance.setValue(jpaFieldInstance.getValue());
        return exchangeFieldInstance;
    }
}

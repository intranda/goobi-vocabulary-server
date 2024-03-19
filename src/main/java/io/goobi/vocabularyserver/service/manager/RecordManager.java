package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldInstance;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.repositories.FieldDefinitionRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRecordRepository;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordManager {
    private final VocabularyRepository vocabularyRepository;
    private final VocabularyRecordRepository vocabularyRecordRepository;
    private final FieldDefinitionRepository fieldDefinitionRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public RecordManager(VocabularyRepository vocabularyRepository,
                         VocabularyRecordRepository vocabularyRecordRepository,
                         FieldDefinitionRepository fieldDefinitionRepository,
                         ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularyRecordRepository = vocabularyRecordRepository;
        this.fieldDefinitionRepository = fieldDefinitionRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
    }

    public List<VocabularyRecord> listAllRecordsOfVocabulary(long id) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Vocabulary", id));
        return jpaVocabulary.getRecords()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    public VocabularyRecord createNewRecordForVocabulary(long id, VocabularyRecord vocabularyRecord) {
        io.goobi.vocabularyserver.model.VocabularyRecord jpaVocabularyRecord = transformVocabularyRecord(id, vocabularyRecord);
        return exchangeTypeTransformer.transform(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    private io.goobi.vocabularyserver.model.VocabularyRecord transformVocabularyRecord(long vocabularyId, io.goobi.vocabularyserver.exchange.VocabularyRecord newVocabularyRecord) {
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = vocabularyRepository
                .findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException("Vocabulary", vocabularyId));
        io.goobi.vocabularyserver.model.VocabularyRecord result = new io.goobi.vocabularyserver.model.VocabularyRecord(jpaVocabulary);
        result.getFields().addAll(newVocabularyRecord.getFields().stream().map(f -> transformFieldInstance(result, findFieldDefinition(f.getDefinitionId()), f)).collect(Collectors.toSet()));
        return result;
    }

    private FieldDefinition findFieldDefinition(long definitionId) {
        return fieldDefinitionRepository.findById(definitionId).orElseThrow(() -> new EntityNotFoundException("FieldDefinition", definitionId));
    }

    // TODO: Perform validation
    private io.goobi.vocabularyserver.model.FieldInstance transformFieldInstance(io.goobi.vocabularyserver.model.VocabularyRecord vocabularyRecord, FieldDefinition fieldDefinition, FieldInstance fieldInstance) {
        io.goobi.vocabularyserver.model.FieldInstance result = new io.goobi.vocabularyserver.model.FieldInstance(fieldDefinition, vocabularyRecord, fieldInstance.getValue());
        result.setLanguage(fieldDefinition.getLanguage());
        return result;
    }
}

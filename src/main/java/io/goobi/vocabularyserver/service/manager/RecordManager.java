package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.FieldInstanceDTO;
import io.goobi.vocabularyserver.exchange.VocabularyRecordDTO;
import io.goobi.vocabularyserver.model.FieldDefinition;
import io.goobi.vocabularyserver.model.FieldInstance;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularyRecord;
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

    public List<VocabularyRecordDTO> listAll(long id) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, id));
        return jpaVocabulary.getRecords()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    public VocabularyRecordDTO get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularyRecordRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(VocabularyRecord.class, id))
        );
    }

    public VocabularyRecordDTO create(long vocabularyId, VocabularyRecordDTO newRecord) {
        VocabularyRecord jpaVocabularyRecord = transformVocabularyRecord(vocabularyId, newRecord);
        return exchangeTypeTransformer.transform(vocabularyRecordRepository.save(jpaVocabularyRecord));
    }

    public VocabularyRecordDTO delete(long id) {
        if (!vocabularyRecordRepository.existsById(id)) {
            throw new EntityNotFoundException(VocabularyRecord.class, id);
        }
        vocabularyRecordRepository.deleteById(id);
        return null;
    }

    private io.goobi.vocabularyserver.model.VocabularyRecord transformVocabularyRecord(long vocabularyId, VocabularyRecordDTO newVocabularyRecordDTO) {
        Vocabulary jpaVocabulary = vocabularyRepository
                .findById(vocabularyId)
                .orElseThrow(() -> new EntityNotFoundException(Vocabulary.class, vocabularyId));
        VocabularyRecord result = new VocabularyRecord(jpaVocabulary);
        result.getFields()
                .addAll(newVocabularyRecordDTO.getFields()
                        .stream()
                        .map(f -> transformFieldInstance(result, findFieldDefinition(f.getDefinitionId()), f))
                        .collect(Collectors.toSet())
                );
        return result;
    }

    private FieldDefinition findFieldDefinition(long definitionId) {
        return fieldDefinitionRepository.findById(definitionId).orElseThrow(() -> new EntityNotFoundException(FieldDefinition.class, definitionId));
    }

    // TODO: Perform validation
    private FieldInstance transformFieldInstance(VocabularyRecord vocabularyRecord, FieldDefinition fieldDefinition, FieldInstanceDTO fieldInstanceDTO) {
        FieldInstance result = new FieldInstance(fieldDefinition, vocabularyRecord, fieldInstanceDTO.getValue());
        result.setLanguage(fieldDefinition.getLanguage());
        return result;
    }
}

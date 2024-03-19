package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularyRecord;
import io.goobi.vocabularyserver.model.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RecordManager {
    private final VocabularyRepository vocabularyRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public RecordManager(VocabularyRepository vocabularyRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularyRepository = vocabularyRepository;
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
}

package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularyManager implements Manager<Vocabulary> {
    private final VocabularyRepository vocabularyRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public VocabularyManager(VocabularyRepository vocabularyRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularyRepository = vocabularyRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
    }

    @Override
    public List<Vocabulary> listAll() {
        return vocabularyRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    @Override
    public Vocabulary create(Vocabulary newVocabulary) {
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = exchangeTypeTransformer.transform(newVocabulary);
        return exchangeTypeTransformer.transform(vocabularyRepository.save(jpaVocabulary));
    }
}

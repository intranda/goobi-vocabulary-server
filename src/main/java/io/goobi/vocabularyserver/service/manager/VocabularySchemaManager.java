package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exchange.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularySchemaManager implements Manager<VocabularySchema> {

    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public VocabularySchemaManager(VocabularySchemaRepository vocabularySchemaRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularySchemaRepository = vocabularySchemaRepository;
        this.exchangeTypeTransformer = exchangeTypeTransformer;
    }

    @Override
    public List<VocabularySchema> listAll() {
        return vocabularySchemaRepository.findAll()
                .stream()
                .map(exchangeTypeTransformer::transform)
                .collect(Collectors.toList());
    }

    @Override
    public VocabularySchema get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularySchemaRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(io.goobi.vocabularyserver.model.VocabularySchema.class, id))
        );
    }
}

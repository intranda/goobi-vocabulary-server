package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
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

    @Override
    public Vocabulary replace(Vocabulary newVocabulary) {
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = vocabularyRepository
                .findById(newVocabulary.getId())
                .orElseGet(() -> exchangeTypeTransformer.transform(newVocabulary));

        List<Runnable> replacements = new LinkedList<>();
        if (newVocabulary.getName() != null) {
            replacements.add(() ->jpaVocabulary.setName(newVocabulary.getName()));
        }
        if (newVocabulary.getDescription() != null) {
            replacements.add(() -> jpaVocabulary.setDescription(newVocabulary.getDescription()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException("Vocabulary", List.of("name", "description"));
        }
        replacements.forEach(Runnable::run);
        return exchangeTypeTransformer.transform(vocabularyRepository.save(jpaVocabulary));
    }

    @Override
    public void delete(long id) {
        if (!vocabularyRepository.existsById(id)) {
            throw new EntityNotFoundException("Vocabulary", id);
        }
        vocabularyRepository.deleteById(id);
    }
}

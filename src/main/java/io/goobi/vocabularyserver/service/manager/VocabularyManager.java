package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
import io.goobi.vocabularyserver.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.model.VocabularySchema;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import io.goobi.vocabularyserver.repositories.VocabularySchemaRepository;
import io.goobi.vocabularyserver.service.exchange.ExchangeTypeTransformer;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class VocabularyManager implements Manager<Vocabulary> {
    private final VocabularyRepository vocabularyRepository;
    private final VocabularySchemaRepository vocabularySchemaRepository;
    private final ExchangeTypeTransformer exchangeTypeTransformer;

    public VocabularyManager(VocabularyRepository vocabularyRepository, VocabularySchemaRepository vocabularySchemaRepository, ExchangeTypeTransformer exchangeTypeTransformer) {
        this.vocabularyRepository = vocabularyRepository;
        this.vocabularySchemaRepository = vocabularySchemaRepository;
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
    public Vocabulary get(long id) {
        return exchangeTypeTransformer.transform(
                vocabularyRepository.findById(id)
                        .orElseThrow(() -> new EntityNotFoundException(io.goobi.vocabularyserver.model.Vocabulary.class, id))
        );
    }

    @Override
    public Vocabulary create(Vocabulary newVocabulary) {
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = transformVocabulary(newVocabulary);
        return exchangeTypeTransformer.transform(vocabularyRepository.save(jpaVocabulary));
    }

    // TODO: This is not working correctly: Impossible to pass id for replace insertion
    @Override
    public Vocabulary replace(Vocabulary newVocabulary, long id) {
        io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary = vocabularyRepository
                .findById(id)
                .orElseThrow(() -> new UnsupportedEntityReplacementException(newVocabulary.getClass(), id));
//                .orElseGet(() -> transformVocabulary(newVocabulary));

        List<Runnable> replacements = new LinkedList<>();
        if (newVocabulary.getName() != null) {
            replacements.add(() -> jpaVocabulary.setName(newVocabulary.getName()));
        }
        if (newVocabulary.getDescription() != null) {
            replacements.add(() -> jpaVocabulary.setDescription(newVocabulary.getDescription()));
        }
        if (replacements.isEmpty()) {
            throw new MissingValuesException(newVocabulary.getClass(), List.of("name", "description"));
        }
        replacements.forEach(Runnable::run);
        return exchangeTypeTransformer.transform(vocabularyRepository.save(jpaVocabulary));
    }

    private io.goobi.vocabularyserver.model.Vocabulary transformVocabulary(Vocabulary newVocabulary) {
        io.goobi.vocabularyserver.model.VocabularySchema jpaVocabularySchema = vocabularySchemaRepository
                .findById(newVocabulary.getSchemaId())
                .orElseThrow(() -> new EntityNotFoundException(VocabularySchema.class, newVocabulary.getSchemaId()));
        io.goobi.vocabularyserver.model.Vocabulary result = new io.goobi.vocabularyserver.model.Vocabulary(jpaVocabularySchema, newVocabulary.getName());
        result.setDescription(newVocabulary.getDescription());
        return result;
    }

    @Override
    public Vocabulary delete(long id) {
        if (!vocabularyRepository.existsById(id)) {
            throw new EntityNotFoundException(io.goobi.vocabularyserver.model.Vocabulary.class, id);
        }
        vocabularyRepository.deleteById(id);
        return null;
    }
}

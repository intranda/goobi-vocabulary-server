package io.goobi.vocabularyserver.api;


import io.goobi.vocabularyserver.exchange.Vocabulary;
import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

@RestController
public class VocabularyController {
    private final VocabularyRepository vocabularyRepository;

    VocabularyController(VocabularyRepository vocabularyRepository) {
        this.vocabularyRepository = vocabularyRepository;
    }

    @GetMapping("/vocabulary")
    List<Vocabulary> getAllVocabularies() {
        return vocabularyRepository.findAll()
                .stream()
                .map(this::transform)
                .collect(Collectors.toList());
    }

    @GetMapping("/schema/{id}")
    Vocabulary getSingleVocabularyById(@PathVariable long id) {
        // TODO: Make nice 404
        return transform(vocabularyRepository.findById(id)
                .orElseThrow());
    }

    private Vocabulary transform(io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary) {
       Vocabulary exchangeVocabulary = new Vocabulary();
        exchangeVocabulary.setId(jpaVocabulary.getId());
        exchangeVocabulary.setName(jpaVocabulary.getName());
        exchangeVocabulary.setDescription(jpaVocabulary.getDescription());
        exchangeVocabulary.setSchemaId(jpaVocabulary.getSchema().getId());
        return exchangeVocabulary;
    }
}

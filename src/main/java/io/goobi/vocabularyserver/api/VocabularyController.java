package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.repositories.VocabularyRepository;
import org.springframework.web.bind.annotation.GetMapping;
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
    public List<io.goobi.vocabularyserver.exchange.Vocabulary> all() {
        return vocabularyRepository.findAll().stream().map(this::transform).collect(Collectors.toList());
    }

    private io.goobi.vocabularyserver.exchange.Vocabulary transform(io.goobi.vocabularyserver.model.Vocabulary jpaVocabulary) {
        io.goobi.vocabularyserver.exchange.Vocabulary exchangeVocabulary = new io.goobi.vocabularyserver.exchange.Vocabulary();
        exchangeVocabulary.setId(jpaVocabulary.getId());
        exchangeVocabulary.setName(jpaVocabulary.getName());
        exchangeVocabulary.setDescription(jpaVocabulary.getDescription());
        exchangeVocabulary.setSchemaId(jpaVocabulary.getSchema().getId());
        return exchangeVocabulary;
    }
}
